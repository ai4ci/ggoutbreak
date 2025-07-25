## `EpiEstim` wrapper ----

#' `EpiEstim` reproduction number wrapper function
#'
#' Calculate a reproduction number estimate from incidence data using the
#' `EpiEstim` library and an empirical generation time distribution. This uses
#' resampling to transmit uncertainty in generation time estimates. This is
#' quite slow for each time series depending on the number of bootstraps and
#' samples in the infectivity profile.
#'
#' This will calculate a reproduction number for each group in the input dataframe.
#'
#' @iparam df Count data. Extra groups are allowed.
#' @iparam ip infectivity profile
#' @param bootstraps - the number of bootstraps to take to calculate for each point.
#' @param window - the width of the `EpiEstim` window
#' @param mean_prior the prior for the $R_t$ estimate. When sample size is low the
#'   $R_t$ estimate will revert to this prior. In `EpiEstim` the default is a high
#'   number to allow detection of insufficient data but this tends to create
#'   anomalies in the early part of infection time series. A possible value is $R_0$
#'   but in fact this also will be a poor choice for the value of $R_t$ when case
#'   numbers drop to a low value.
#' @param std_prior the prior for the $R_t$ SD.
#' @param ... not used
#' @param .progress show a CLI progress bar
#'
#' @return `r i_reproduction_number`
#' @export
#' @concept models
#' @examples
#'
#' data = test_poisson_rt_smooth
#'
#' tmp2 = data %>%
#'    ggoutbreak::rt_epiestim(test_ip)
#'
#' if (interactive()) {
#'   plot_rt(tmp2, date_labels="%b %y")+sim_geom_function(data,colour="red")
#' }
#'
rt_epiestim = function(
  df = i_incidence_input,
  ip = i_discrete_ip,
  bootstraps = 2000,
  window = 14,
  mean_prior = 1,
  std_prior = 2,
  ...,
  .progress = interactive()
) {
  ip = interfacer::ivalidate(ip)
  ip_boots = dplyr::n_distinct(ip$boot)

  siConfig = EpiEstim::make_config(
    method = "si_from_sample",
    mean_prior = mean_prior,
    std_prior = std_prior
  )

  siConfig$n2 = max(bootstraps %/% ip_boots, 10)

  env = rlang::current_env()
  if (.progress) {
    cli::cli_progress_bar(
      "Rt (EpiEstim)",
      total = dplyr::n_groups(df),
      .envir = env
    )
  }

  modelled = interfacer::igroup_process(
    df,
    function(df, siConfig, ip, window, .groupdata, ...) {
      .stop_if_not_daily(df$time)

      meta = .get_meta(df$time)
      tmp = df %>% dplyr::transmute(I = count)
      siConfig$t_start = c(2:(nrow(tmp) - window))
      siConfig$t_end = siConfig$t_start + window

      # find the right ip for this group.
      yMatrix = .select_ip(ip, .groupdata) %>%
        .omega_matrix(epiestim_compat = TRUE)

      rt.warn = NA

      tmp4 =
        withCallingHandlers(
          tryCatch(
            EpiEstim::estimate_R(
              tmp,
              method = "si_from_sample",
              config = siConfig,
              si_sample = yMatrix
            ),
            error = stop
          ),
          warning = function(w) {
            rt.warn <<- w$message
            invokeRestart("muffleWarning")
          }
        )

      new_data = tmp4$R %>%
        dplyr::transmute(
          time = as.time_period(t_end, meta$unit, meta$start_date),
          rt.fit = `Mean(R)`,
          rt.se.fit = `Std(R)`,
          rt.0.025 = `Quantile.0.025(R)`,
          rt.0.05 = `Quantile.0.05(R)`,
          rt.0.25 = `Quantile.0.25(R)`,
          rt.0.5 = `Median(R)`,
          rt.0.75 = `Quantile.0.75(R)`,
          rt.0.95 = `Quantile.0.95(R)`,
          rt.0.975 = `Quantile.0.975(R)`,
          rt.warn = rt.warn
        ) %>%
        .keep_cdf(type = "rt", link = "log") # infers from quantiles.

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
