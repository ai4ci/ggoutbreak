

#' Rescale a timeseries in the temporal dimension
#'
#' Sometimes we may have, for example, modelled incidence or growth rates on
#' weekly data resulting in cases per week and growth rate per week. We may wish
#' to use this to estimate the reproduction number, using algorithms that assume
#' a daily incidence. Not everything has a dependence on time, and things such
#' as proportions, or prevalence will not change.
#'
#' @param df A data frame containing modelled output. This will modify the
#' following columns if present:
#'
#'  `r i_incidence_model`
#'
#'  `r i_growth_rate`
#'
#'  `r i_proportion_rate`
#' @param time_unit a `lubridate` period string such as "1 day"
#'
#' @return the same time series with different time unit, and adjusted incidence
#'   and growth rate figures.
#'
#' @export
#' @concept models
#'
#' @examples
#'
#' sim = sim_poisson_model(time_unit = "1 week")
#' incidence = sim %>% poisson_locfit_model(frequency = "1 day", deg = 2, window=5)
#' incidence2 = incidence %>% rescale_model(time_unit = "1 day")
#' incidence2 %>% dplyr::glimpse()
#'
rescale_model = function(df = i_timeseries, time_unit) {

  df = interfacer::ivalidate(df)
  time_unit = lubridate::as.period(time_unit)

  # The time change fraction
  timefrac = time_unit/attr(df$time,"unit")
  df = df %>% dplyr::mutate(time = as.time_period(time, unit=time_unit))

  if ( interfacer::itest(df, i_incidence_data) ) {
    warning("Rescaling does not aggregate or interpolate count columns.",call. = FALSE)
  }

  # Incidence:
  # incidence has an implicit rate depending on the time unit of the
  # input data. Rescaling this is needed

  if ( interfacer::itest(df, i_incidence_model) ) {
      df = df %>% dplyr::mutate(
        incidence.fit = incidence.fit + log(timefrac),
        incidence.se.fit = incidence.se.fit, # this is lognormal dist
        incidence.0.025 = incidence.0.025 * timefrac,
        incidence.0.5 = incidence.0.5 * timefrac,
        incidence.0.975 = incidence.0.975 * timefrac
      )
  }

  # Growth rates:
  # timefrac will be something like 1/7 if going from weekly to daily
  # absolute growth will be ^1/7, exponential rate will be *1/7

  if ( interfacer::itest(df, i_growth_rate) ) {
    df = df %>% dplyr::mutate(
      growth.fit = growth.fit*timefrac,
      growth.se.fit = growth.se.fit*timefrac, # this is normal dist
      growth.0.025 = growth.0.025*timefrac,
      growth.0.5 = growth.0.5*timefrac,
      growth.0.975 = growth.0.975*timefrac
    )
  }

  if ( interfacer::itest(df, i_proportion_rate) ) {
    df = df %>% dplyr::mutate(
      relative.growth.fit = relative.growth.fit*timefrac,
      relative.growth.se.fit = relative.growth.se.fit*timefrac, # this is due to normal distribution
      relative.growth.0.025 = relative.growth.0.025*timefrac,
      relative.growth.0.5 = relative.growth.0.5*timefrac,
      relative.growth.0.975 = relative.growth.0.975*timefrac
    )
  }

  # We do not need to rescale the following as they are time invariant
  # Proportion models
  # Incidence per capita. The time rescaling is already done by "normalise_incidence"
  # Risk ratio models
  # Reproduction number

  return(df)

}


