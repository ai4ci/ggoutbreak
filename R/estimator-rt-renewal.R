.finite = function(f) {
  return(function(x, ...) {
    if (is.null(x) || !all(is.finite(x))) {
      return(NA)
    }
    suppressWarnings(f(x, ...))
  })
}

#' Reproduction number from renewal equation applied to modelled incidence
#' using statistical re-sampling
#'
#' Calculate a reproduction number estimate from modelled incidence estimates,
#' by statistical sampling from a log-normally distributed incidence estimate,
#' such as you get from a `I_t ~ Poisson(I_0 e^{rt})`  model using a log link
#' function. This is combined with uncertain infectivity profile specified as
#' multiple discrete empirical distributions, to calculate the range of possible
#' values for $R_t$.
#'
#' This method is moderately slow and non deterministic by default.
#'
#' @iparam df modelled incidence estimate
#' @iparam ip infectivity profile
#' @param bootstraps the number of samples to take at each time point. This will
#'   be rounded up to a whole multiple of the infectivity profile distribution
#'   length.
#' @param seed a random number seed for reproducibility
#' @param .progress show a CLI progress bar
#'
#' @return `r i_reproduction_number`
#' @export
#' @concept models
#' @examples
#' data = example_poisson_rt_smooth()
#'
#' tmp2 = data %>%
#'    poisson_locfit_model() %>%
#'    rt_from_renewal(ip=example_ip())
#'
#' if (interactive()) {
#'   plot_rt(tmp2, date_labels="%b %y")+sim_geom_function(data,colour="red")
#' }
#'
rt_from_renewal = function(
  df = i_incidence_model,
  ip = i_discrete_ip,
  bootstraps = 1000,
  seed = Sys.time(),
  .progress = interactive()
) {
  #, assume_start = TRUE) {

  ip = interfacer::ivalidate(ip)

  #test negative serial interval
  start = min(ip$tau)

  #TODO: nest into igroup_rprocess
  # omega is a matrix 13x100
  omega = ip %>% omega_matrix(epiestim_compat = FALSE)

  window = nrow(omega)
  # at a minimum we sample 10 times per infectivity profile.
  reps = min(ceiling(bootstraps / ncol(omega)), 10)
  boots = reps * ncol(omega)

  # This reverses order so that the convolution can happen correctly without
  # reordering the timeseries.
  logomega = log(omega)[window:1]
  #TODO: nest into igroup_rprocess ^^^

  env = rlang::current_env()
  if (.progress) {
    cli::cli_progress_bar(
      "Rt (renewal)",
      total = dplyr::n_groups(df),
      .envir = env
    )
  }

  modelled = interfacer::igroup_process(
    df,
    function(df, logomega, window, ...) {
      .stop_if_not_daily(df$time)
      pad = .ln_pad(
        length = window,
        time = df$time,
        mu = df$incidence.fit,
        sigma = df$incidence.se.fit,
        spread = 1.1
      )

      tmp = withr::with_seed(seed, {
        dplyr::tibble(
          time = pad$time,
          incidence.fit = pad$mu,
          incidence.se.fit = pad$sigma,
          imputed = pad$imputed
        ) %>%
          dplyr::mutate(
            incidence.logsamples = purrr::map2(
              incidence.fit,
              incidence.se.fit,
              ~ stats::rnorm(boots, .x, .y)
            )
          )
      })

      end = nrow(tmp) + min(c(start, 0))

      tmp$rt.samples = vector("list", nrow(tmp))
      for (i in seq_along(tmp$rt.samples)) {
        if (i >= window + start && i <= end) {
          # incidence window is 13x1000. start is typically 1, zero or potentially negative.
          window_subset = tmp$incidence.logsamples[
            (i - window - start + 1):(i - start)
          ]
          samples_window = matrix(
            unlist(window_subset),
            nrow = window,
            byrow = TRUE
          )
          samples_window_ip = samples_window + as.vector(logomega)
          samples_force = apply(samples_window_ip, 2, .logsumexp)
          tmp$rt.samples[[i]] = exp(
            tmp$incidence.logsamples[[i]] - as.vector(samples_force)
          )
        }
      }

      new_data = tmp %>%
        dplyr::filter(!imputed) %>%
        dplyr::mutate(
          rt.fit = purrr::map_dbl(rt.samples, .finite(mean)),
          rt.se.fit = purrr::map_dbl(rt.samples, .finite(stats::sd))
        ) %>%
        .result_from_fit(
          "rt",
          # This format is needed because quantile is not vectorised on data:
          qfn = function(p) {
            purrr::map_dbl(.$rt.samples, function(data) {
              stats::quantile(data, p)
            })
          }
        ) %>%
        .keep_cdf(type = "rt", .$rt.samples, link = "log") %>%
        dplyr::select(-rt.samples, -incidence.logsamples, -imputed)

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
