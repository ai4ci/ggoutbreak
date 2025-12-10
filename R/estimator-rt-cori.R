## `EpiEstim` wrapper ----

#' Reproduction number estimate using the Cori method
#'
#' Calculate a reproduction number estimate from incidence data using a reimplementation
#' of the Cori method and an empirical generation time distribution. This uses
#' a mixture distribution to transmit uncertainty in generation time estimates.
#' A number of changes compared to the original `EpiEstim` implementation
#' have been made. Firstly there is no technical
#' limitation to the infectivity profile being strictly positive in time. This allows
#' use of serial intervals and secondary potentially delayed observations.
#' Secondly this implementation should tolerate missing count values (NA values
#' must be filtered out though). Thirdly for a given time point `t` this applies
#' all `Rt` estimates for which the window spans time point `t` rather than end
#' on time point `t`, which tends to address lag issues with the original,
#' and fourthly this implementation allows multiple window
#' widths to be calculated in parallel and aggregated. All of this tends to
#' increase uncertainty in the result particularly in the time dimension, which
#' addresses some of the issue seem with `EpiEstim` during the pandemic. Finally
#' it is quite a bit quicker, especially if approximate quantiles are all that
#' are needed.
#'
#' There are still issues with large $R_t$ estimates in the early part of a
#' time series, which is a resul tof the renewal equaltion method.
#'
#' This will calculate a reproduction number for each group in the input dataframe.
#'
#'
#'
#' @iparam df The count data. Extra groups are allowed.
#' @iparam ip A long format infectivity profile.
#' @param window - the widths of the Cori method window to include in the estimate.
#'   This can be a vector of values and all windows will be calculated and aggregated.
#' @param mean_prior the prior for the $R_t$ estimate. When sample size is low the
#'   $R_t$ estimate will revert to this prior. In `EpiEstim` the default is a high
#'   number to allow detection of insufficient data but this tends to create
#'   anomalies in the early part of infection time series. A possible value is $R_0$
#'   but in fact this also will be a poor choice for the value of $R_t$ when case
#'   numbers drop to a low value.
#' @param std_prior the prior for the $R_t$ SD.
#' @param ... not used
#' @param epiestim_compat produce an estimate of `Rt` using only windows that end
#'   on the time `t` rather than all windows that span time `t`. If this option is
#'   selected there can also only be one value for window.
#' @param approx approximate the quantiles of the mixture distribution with a
#'   gamma distribution with the same first mean and SD.
#' @param .progress show a CLI progress bar
#'
#' @return `r i_reproduction_number`
#' @export
#' @concept models
#' @examples
#'
#' data = example_poisson_rt_smooth()
#'
#' tmp2 = data %>% rt_cori(ip=example_ip(), epiestim_compat = TRUE)
#' tmp3 = data %>% rt_cori(ip=example_ip(), window=c(5:14), approx=TRUE)
#'
#' comp = dplyr::bind_rows(
#'   tmp2 %>% dplyr::mutate(class = "EpiEstim"),
#'   tmp3 %>% dplyr::mutate(class = "Cori+")
#' ) %>% dplyr::group_by(class)
#'
#' if (interactive()) {
#'   plot_rt(comp, date_labels="%b %y")+sim_geom_function(data,colour="black")+
#'     ggplot2::coord_cartesian(ylim=c(0.5,3.0))
#' }
#'
rt_cori = function(
  df = i_incidence_input,
  ip = i_discrete_ip,
  window = 14,
  mean_prior = 1,
  std_prior = 2,
  ...,
  epiestim_compat = FALSE,
  approx = FALSE,
  .progress = interactive()
) {
  #TODO: slow example

  if (any(window < 2)) {
    stop("Minimum value for `window` parameter is 2.")
  }
  if (epiestim_compat) {
    window = window[1]
  }

  ip = interfacer::ivalidate(ip)
  ip_boots = dplyr::n_distinct(ip$boot)

  shape_prior = mean_prior^2 / std_prior^2
  rate_prior = mean_prior / std_prior^2

  env = rlang::current_env()
  if (.progress) {
    cli::cli_progress_bar(
      "Rt (Cori+)",
      total = dplyr::n_groups(df),
      .envir = env
    )
  }

  modelled = interfacer::igroup_process(
    df,
    function(df, window, .groupdata, ...) {
      .stop_if_not_daily(df$time)

      df2 = df %>%
        dplyr::cross_join(
          .select_ip(ip, .groupdata) %>%
            dplyr::rename(omega_t_tau = probability)
        ) %>%
        dplyr::group_by(boot)

      # Calculate the FOI (Lambda_t) for every timepoint (and boot)
      df3 = df2 %>%
        dplyr::mutate(
          lambda_t_tau = count * omega_t_tau,
          t_tau = time + tau,
        ) %>%
        dplyr::group_by(boot, t = t_tau) %>%
        dplyr::summarise(
          n_t = dplyr::n(),
          Lambda_t = sum(lambda_t_tau)
        )

      df3 = df3 %>%
        dplyr::inner_join(
          df %>% dplyr::rename(t = time, I_t = count),
          by = "t"
        )

      # We allow multiple windows
      df4 = dplyr::bind_rows(lapply(window, function(win) {
        df3 %>%
          dplyr::cross_join(dplyr::tibble(s = 1:win)) %>%
          dplyr::mutate(t_end = t + s) %>%
          dplyr::group_by(boot, t_end) %>%
          dplyr::summarise(
            sum_n_s = sum(n_t),
            sum_I_s = sum(I_t),
            sum_Lambda_s = sum(Lambda_t)
          ) %>%
          dplyr::mutate(
            t_start = t_end - win
          )
      }))

      df4 = df4 %>%
        dplyr::mutate(
          shape_post = shape_prior + sum_I_s,
          rate_post = rate_prior + sum_Lambda_s, # N.B. Original method parametrised in scale not rate
          mean_post = shape_post / rate_post,
          var_post = shape_post / rate_post^2
        )

      # Each posterior estimate is based on assumption that R_t is constant
      # between t_start and t_end. Therefore if the value of t is between these
      # two the estimate is relevant to that time point.
      if (!epiestim_compat) {
        by = dplyr::join_by(dplyr::between(time, t_start, t_end))
      } else {
        # The original epiestim though does not do this and only estimates from
        # each IP bootstrap aligend to the end of the window are applicable.
        by = c("time" = "t_end")
      }

      df5 = df %>%
        dplyr::inner_join(df4, by = by) %>%
        dplyr::group_by(time)

      if (!approx) {
        df6 = df5 %>%
          dplyr::summarise(
            rt.fit = mean(mean_post),
            rt.se.fit = sqrt(mean(var_post + mean_post^2) - rt.fit^2),
            rt.shapes = list(shape_post),
            rt.rates = list(rate_post) #,
            # rt.true = if (interfacer::is_col_present(.,rt.true)) unique(rt.true) else NULL
          ) %>%
          .result_from_fit(
            type = "rt",
            qfn = function(p) {
              .qmixlist(
                dist = "gamma",
                p = p,
                param1list = .$rt.shapes,
                param2list = .$rt.rates,
                method = "exact"
              )
            }
          ) %>%
          .keep_cdf(
            type = "rt",
            shape = .$rt.shapes,
            rate = .$rt.rates,
            link = "log"
          ) %>%
          dplyr::select(-c(rt.rates, rt.shapes))
      } else {
        df6 = df5 %>%
          dplyr::summarise(
            # calculate the mean & sd of the mixture.
            rt.fit = mean(mean_post),
            rt.se.fit = sqrt(mean(var_post + mean_post^2) - rt.fit^2) #,
            # rt.true = if (interfacer::is_col_present(.,rt.true)) unique(rt.true) else NULL
          ) %>%
          dplyr::mutate(
            rt.shape = rt.fit^2 / rt.se.fit^2,
            rt.rate = rt.fit / rt.se.fit^2
          ) %>%
          .result_from_fit(
            type = "rt",
            qfn = function(p) qgamma2(p, .$rt.fit, .$rt.se.fit)
          ) %>%
          .keep_cdf(
            type = "rt",
            shape = .$rt.shape,
            rate = .$rt.rate,
            link = "log"
          ) %>%
          dplyr::select(-c(rt.rate, rt.shape))
      }

      new_data = df6

      if (.progress) {
        cli::cli_progress_update(.envir = env)
      }

      return(new_data)
    }
  )

  if (.progress) {
    cli::cli_progress_done()
  }

  return(modelled)
}
