#' Calculate a normalised incidence rate per capita
#'
#' This assumes positive disease counts are stratified by a population grouping,
#' e.g. geography or age, and we have estimates of the size of that population
#' during that time period. Normalising by population size allows us to compare
#' groups.
#'
#' @iparam pop The population data must be grouped in the same way as `modelled`.
#' @iparam modelled Model output from processing the `raw` dataframe with
#'   something like `poission_locfit_model`
#' @param ... not used
#' @param population_unit what population unit do you want the incidence in e.g.
#'   per 100K
#' @param normalise_time The default behaviour for incidence is to keep it in
#'   the same time units as the input data. If this parameter is set to `TRUE`
#'   the incidence rates are calculated per year. If given as a lubridate period
#'   string e.g. "1 day" then the incidence is calculated over that time period.
#'
#' @return a dataframe with incidence rates per unit capita.
#'   `r i_incidence_per_capita_model`
#' @export
#' @concept models
#'
#' @examples
#'
#' model = example_poisson_age_stratified()
#' demog = ukc19::uk_population_2019_by_5yr_age
#'
#' model %>%
#'   normalise_incidence(demog) %>%
#'   dplyr::glimpse()
#'
normalise_incidence = function(
  modelled = i_incidence_model,
  pop = i_population_data,
  ...,
  population_unit = 100000,
  normalise_time = FALSE
) {
  modelled = interfacer::ivalidate(modelled)
  modelled = infer_population(modelled, pop)

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
    timefrac = out_unit / unit
  }

  # interfacer::ireturn(
  modelled %>%
    dplyr::mutate(
      incidence.per_capita.0.025 = incidence.0.025 /
        population *
        population_unit *
        timefrac,
      incidence.per_capita.0.5 = incidence.0.5 /
        population *
        population_unit *
        timefrac,
      incidence.per_capita.0.975 = incidence.0.975 /
        population *
        population_unit *
        timefrac,
      incidence.per_capita.fit = incidence.fit -
        log(population / population_unit) +
        log(timefrac),
      incidence.per_capita.se.fit = incidence.se.fit,
      # no transformation under log normal assumption
      population_unit = population_unit,
      time_unit = out_unit
    )
  #  i_incidence_per_capita_model
  #)
}


#' Calculate a normalised count per capita
#'
#' This assumes positive disease counts are stratified by a population grouping,
#' e.g. geography or age, and we have estimates of the size of that population
#' during that time period. Normalising by population size allows us to compare
#' groups.
#'
#' @iparam pop The population data must be grouped in the same way as `raw`.
#' @iparam raw The count data
#' @param ... not used
#' @param population_unit What population unit do you want the count data
#'   normalised to e.g. per 100K
#' @param normalise_time The default behaviour for normalising is to keep it in
#'   the same time units as the input data. If this parameter is set to `TRUE`
#'   the incidence rates are calculated per year. If given as a lubridate period
#'   string e.g. "1 week" then the incidence is calculated over that time
#'   period.
#'
#' @return a dataframe with incidence rates per unit capita.
#'   `r i_incidence_per_capita_data`
#' @export
#' @concept models
#'
#' @examples
#'
#' data = example_england_covid_by_age()
#' demog = ukc19::uk_population_2019_by_5yr_age
#'
#' data %>%
#'   normalise_count(demog) %>%
#'   dplyr::glimpse()
#'
normalise_count = function(
  raw = i_incidence_data,
  pop = i_population_data,
  ...,
  population_unit = 100000,
  normalise_time = FALSE
) {
  raw = interfacer::ivalidate(raw)
  raw = infer_population(raw, pop)

  unit = .get_meta(raw$time)$unit
  if (isFALSE(normalise_time)) {
    out_unit = unit
    timefrac = 1
  } else {
    if (isTRUE(normalise_time)) {
      out_unit = lubridate::as.period("1 year")
    } else {
      out_unit = lubridate::as.period(normalise_time)
    }
    timefrac = out_unit / unit
  }

  # interfacer::ireturn(
  raw %>%
    dplyr::mutate(
      count.per_capita = count / population * population_unit * timefrac,
      population_unit = population_unit,
      time_unit = out_unit
    )
  #  i_incidence_per_capita_data
  #)
}

# normalise the incidence extracting pop from input data.
.normalise_from_raw = function(modelled, d) {
  if (interfacer::is_col_present(d, population, population_unit, time_unit)) {
    population_unit = unique(d$population_unit)
    out_unit = d$time_unit[[1]]
    # recreate population (including grouping) from input data
    pop = d %>% dplyr::select(time, population)

    # could extract a pop dataframe with time and population from d.
    # potentially could do this outside of group_modify.
    unit = .get_meta(modelled$time)$unit

    timefrac = out_unit / unit

    modelled = modelled %>%
      infer_population(pop) %>%
      dplyr::mutate(
        incidence.per_capita.0.025 = incidence.0.025 /
          population *
          population_unit *
          timefrac,
        incidence.per_capita.0.5 = incidence.0.5 /
          population *
          population_unit *
          timefrac,
        incidence.per_capita.0.975 = incidence.0.975 /
          population *
          population_unit *
          timefrac,
        incidence.per_capita.fit = incidence.fit -
          log(population / population_unit) +
          log(timefrac),
        incidence.per_capita.se.fit = incidence.se.fit,
        population_unit = population_unit,
        time_unit = out_unit
      )
  }
  return(modelled)
}
