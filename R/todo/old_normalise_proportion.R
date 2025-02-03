#' Calculate a normalised incidence rate per capita
#'
#' This assumes positive disease counts are stratified by a population grouping, e.g.
#' geography or age, and we have estimates of the size of that population during
#' that time period. Normalising by population size allows us to compare groups.
#'
#' This scales a proportion model by the population unit to make it comparable to
#' an incidence model.
#'
#' @iparam modelled Model output from processing the `raw` dataframe with something like
#'   `poission_locfit_model`
#' @param ... not used
#' @param population_unit what population unit do you want the incidence in e.g. per 100K
#' @param normalise_time The default behaviour for incidence is to keep it in
#'   the same time units as the input data. If this parameter is set to `TRUE` the
#'   incidence rates are calculated per year. If given as a lubridate period string
#'   e.g. "1 day" then the incidence is calculated over that time period.
#'
#' @return a dataframe with incidence rates per unit capita.
#'   `r i_incidence_per_capita_model`
#' @export
#' @concept models
#'
#' @examples
#' tmp = ggoutbreak::england_covid %>%
#'   ggoutbreak::poisson_locfit_model(window=21) %>%
#'   ggoutbreak::normalise_incidence(ggoutbreak::england_demographics) %>%
#'   dplyr::glimpse()
#'
normalise_incidence.proportion = function(
    modelled = i_proportion_model,
    ...,
    population_unit = 100000,
    normalise_time = FALSE
) {

  unit = .get_meta(modelled$time)$unit
  if (isFALSE(normalise_time)) {
    out_unit = unit
    timefrac = 1
  } else {
    if (isTRUE(normalise_time)) {
      out_unit = lubridate::as.period("1 year")
    } else {
      out_unit = lubridate::as.period(normalise_time)
    }
    timefrac = out_unit/unit
  }

  # proportion models just need to be

  # interfacer::ireturn(
  modelled %>%
    dplyr::mutate(
      incidence.per_capita.0.025 = proportion.0.025 * population_unit * timefrac,
      incidence.per_capita.0.5 = proportion.0.5 * population_unit * timefrac,
      incidence.per_capita.0.975 = proportion.0.975 * population_unit * timefrac,
      incidence.per_capita.fit = log(.expit(proportion.fit)) + log(population_unit) + log(timefrac),
      incidence.per_capita.se.fit = log(.expit(proportion.se.fit)) + log(population_unit) + log(timefrac),
      population_unit = population_unit,
      time_unit = out_unit
    )
  #  i_incidence_per_capita_model
  #)


}
