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
#' data = example_poisson_growth_rate()
#' tmp2 = data %>%
#'   poisson_glm_model(window=7,deg=1) %>%
#'   growth_rate_from_incidence(window = 13, deg=1)
#'
#' if(interactive()) {
#'   plot_incidence(tmp2,
#'       date_labels="%b %y")
#'
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
    stop("Window must be odd and >=3.")
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
    dplyr::group_modify(function(d2, g2, ...) {
      d2 %>%
        dplyr::mutate(
          growth.fit = signal::filter(sg_filter, incidence.fit),
          growth.se.fit = sqrt(signal::filter(sg_filter_2, incidence.se.fit^2))
        ) %>%
        .result_from_fit("growth")
    })
}


#' Estimate relative growth rate from modelled proportion
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This assumes a modelled proportion that is logit-normally distributed.
#' The exponential growth rate is the first derivative of the mu parameters of this
#' logit-normal. On the link scale these are normally distributed. This function
#' assumes that the time series incidence estimates are uncorrelated to estimate
#' the error in the growth rate, which is a conservative approach resulting in
#' more uncertainty in growth rate than might be possible through other methods.
#' This is all based on Savitsky-Golay filters applied to the normally
#' distributed logit-proportion estimates.
#'
#' @iparam d a modelled proportion estimate
#' @param window the width of the Savitsky-Golay filter - must be odd
#' @param deg the polynomial degree of the filter
#'
#' @returns the timeseries with growth rate columns: `r i_proportion_rate`
#' @export
#' @concept models
#'
#' @examples
#' data = example_poisson_rt_2class()
#'
#' tmp2 = data %>%
#'   proportion_glm_model(window=7,deg=2) %>%
#'   growth_rate_from_proportion(window = 13, deg=1)
#'
#' if(interactive()) {
#'   plot_proportion(tmp2,
#'       date_labels="%b %y")
#'
#'   plot_growth_rate(
#'       tmp2,
#'       date_labels="%b %y"
#'     )
#' }
growth_rate_from_proportion = function(
  d = i_proportion_model,
  window = 7,
  deg = 2
) {
  d = interfacer::ivalidate(d)

  delta = as.numeric(attributes(d$time)$unit) / (24 * 60 * 60)
  lengths = d %>% dplyr::count() %>% dplyr::pull(n)
  if (window %% 2 == 0 || window < 3) {
    stop("Window must be odd and >= 3.")
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
    dplyr::group_modify(function(d2, g2, ...) {
      d2 %>%
        dplyr::mutate(
          relative.growth.fit = signal::filter(sg_filter, proportion.fit),
          relative.growth.se.fit = sqrt(signal::filter(
            sg_filter_2,
            proportion.se.fit^2
          ))
        ) %>%
        .result_from_fit("relative.growth")
    })
}


#' Estimate relative growth rate from estimated prevalence
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This assumes a prevalence that is logit-normally distributed.
#' The exponential growth rate is the first derivative of the mu parameters of this
#' logit-normal. On the link scale these are normally distributed. This function
#' assumes that the time series incidence estimates are uncorrelated to estimate
#' the error in the growth rate, which is a conservative approach resulting in
#' more uncertainty in growth rate than might be possible through other methods.
#' This is all based on Savitsky-Golay filters applied to the normally
#' distributed logit-proportion estimates.
#'
#' @iparam d a modelled proportion estimate
#' @param window the width of the Savitsky-Golay filter - must be odd
#' @param deg the polynomial degree of the filter
#'
#' @returns the timeseries with growth rate columns: `r i_proportion_rate`
#' @export
#' @concept models
#'
#' @examples
#'
#' data = example_poisson_rt_2class()
#'
#' tmp = data %>%
#'   proportion_glm_model(window=7,deg=2) %>%
#'   dplyr::select(time, dplyr::starts_with("proportion")) %>%
#'   dplyr::rename_with(~ gsub("proportion","prevalence",.x)) %>%
#'   dplyr::select(-prevalence.fit, -prevalence.se.fit)
#'
#' tmp = tmp %>%
#'   growth_rate_from_prevalence(window = 25, deg=1)
#'
#' plot_growth_rate(
#'       tmp,
#'       date_labels="%b %y"
#'   )
#'
#' data1 = ukc19::ons_infection_survey %>%
#'   dplyr::filter(name=="England") %>%
#'   timeseries(count=FALSE)
#'
#' tmp2 = data1 %>%
#'   growth_rate_from_prevalence(window = 25, deg=1)
#'
#' if(interactive()) {
#'   plot_growth_rate(
#'       tmp2,
#'       date_labels="%b %y",
#'       mapping = ggplot2::aes(colour=name),
#'       events = ukc19::timeline
#'   )
#' }
growth_rate_from_prevalence = function(
  d = i_prevalence_model,
  window = 7,
  deg = 2
) {
  d = interfacer::ivalidate(d)

  d = d %>%
    .quantile_apply("prevalence", function(p, q) {
      .infer_normal(p, q, "logit") %>%
        dplyr::rename(prevalence.fit = mean, prevalence.se.fit = sd)
    })

  delta = as.numeric(attributes(d$time)$unit) / (24 * 60 * 60)
  lengths = d %>% dplyr::count() %>% dplyr::pull(n)
  if (window %% 2 == 0 || window < 3) {
    stop("Window must be odd and >= 3.")
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
    dplyr::group_modify(function(d2, g2, ...) {
      d2 %>%
        dplyr::mutate(
          growth.fit = signal::filter(sg_filter, prevalence.fit),
          growth.se.fit = sqrt(signal::filter(
            sg_filter_2,
            prevalence.se.fit^2
          ))
        ) %>%
        .result_from_fit("growth")
    })
}
