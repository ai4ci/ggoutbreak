#' Estimate growth rate from modelled incidence
#'
#' This assumes a modelled incidence estimate that is log-normal. The
#' exponential growth rate is the first derivative of the mu parameters of this
#' log-normal. On the link scale these are normally distributed. This function
#' assumes that the time series incidence estimates are uncorrelated to estimate
#' the error in the growth rate, which is a conservative approach resulting in
#' more uncertainty in growth rate than might be possible through other methods.
#' This is all based on Savitsky-Golay filters applied to the normally
#' distributed log-incidence estimates.
#'
#' @iparam d a modelled incidence estimate
#' @param window the width of the Savitsky-Golay filter - must be odd
#' @param deg the polynomial degree of the filter
#'
#' @returns the timeseries with growth rate columns: `r i_growth_rate`
#' @export
#' @concept models
#'
#' @examples
#' data = test_poisson_growth_rate
#' tmp2 = data %>%
#'   poisson_glm_model(window=7,deg=1) %>%
#'   growth_rate_from_incidence(window = 13, deg=1)
#'
#' if(interactive()) {
#'   plot_growth_rate(
#'       tmp2,
#'       date_labels="%b %y"
#'     )+
#'     sim_geom_function(data,colour="red")
#' }
growth_rate_from_incidence = function(
  d = i_incidence_model,
  window = 7,
  deg = 2
) {
  d = interfacer::ivalidate(d)

  delta = as.numeric(attributes(d$time)$unit) / (24 * 60 * 60)
  lengths = d %>% dplyr::count() %>% dplyr::pull(n)
  if (window %% 2 == 0 || window < 3) {
    stop("Window must be odd.")
  }
  if (any(lengths < window)) {
    stop("Not enough data to estimate growth rate.")
  }

  sg_filter = signal::sgolay(
    p = deg,
    n = window,
    m = 1,
    ts = delta
  )

  sg_filter_2 = sg_filter^2

  d %>%
    dplyr::mutate(
      growth.fit = signal::filter(sg_filter, incidence.fit),
      growth.se.fit = sqrt(signal::filter(sg_filter_2, incidence.se.fit^2))
    ) %>%
    .result_from_fit("growth")
}
