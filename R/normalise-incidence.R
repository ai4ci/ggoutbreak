
#' Calculate a normalised incidence rate per capita
#'
#' This assumes positive disease counts are stratified by a population grouping, e.g.
#' geography or age, and we have estimates of the size of that population during
#' that time period. Normalising by population size allows us to compare groups.
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
normalise_incidence = function(
    modelled = i_timeseries,
    ...,
    population_unit = 100000,
    normalise_time = FALSE
) {

  if (interfacer::is.iface(modelled)) modelled = raw %>% dplyr::group_modify(poisson_locfit_model, ...)

  interfacer::idispatch(modelled,
                        normalise_incidence.proportion = i_proportion_model,
                        normalise_incidence.incidence = i_incidence_model
  )

}

#' Calculate a normalised incidence rate per capita
#'
#' This assumes positive disease counts are stratified by a population grouping, e.g.
#' geography or age, and we have estimates of the size of that population during
#' that time period. Normalising by population size allows us to compare groups.
#'
#' @iparam pop The population data must be grouped in the same way as `modelled`.
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
normalise_incidence.incidence = function(
    modelled = i_incidence_model,
    pop = i_population_data,
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


  pop = interfacer::ivalidate(pop, ...)
  modelled = interfacer::ivalidate(modelled, ...)
  if (!all(dplyr::group_vars(pop) %in% dplyr::group_vars(modelled)))
    stop(
      "different column groupings in `modelled` and `pop` parameters.\n",
      "`modelled` must be compatible from the `pop` data"
    )

  # population is either going to be a dataframe of population in each
  # region or area, or a grouped timeseries

  if (interfacer::itest(pop, i_timeseries)) {

    # population is a timeseries

    if(all(modelled$time %in% pop$time)) {

      # The timeseries is aligned with the modelled timeseries.
      # we don't need to interpolate anything
      modelled = modelled %>%
        dplyr::left_join(
          pop, by = c(dplyr::group_vars(pop),"time")
        ) %>%
        dplyr::mutate(population_unit = population_unit)

    } else {

      # pop is given as a timeseries and we need to interpolate values
      # this following calculates a function for each group that allows
      # a groupwise time based iterpolation based on a loess function
      adj = pop %>% dplyr::reframe(
        population_fn = list(.loessfn(x = time, y=population))
      )
      modelled = modelled %>%
        dplyr::group_modify(function(d,g,...) {

          pop_fn = adj %>%
            dplyr::semi_join(g, by=colnames(g)) %>%
            dplyr::pull(population_fn)

          if (length(pop_fn)==0) pop_fn = function(x) rep(NA,length(x))
          else pop_fn = pop_fn[[1]]

          d %>% dplyr::mutate(population = pop_fn(time))
        }) %>%
        dplyr::mutate(population_unit = population_unit)
      }
  } else {

    # pop is static.
    modelled = modelled %>%
      dplyr::inner_join(pop, by=dplyr::group_vars(pop)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(dplyr::group_vars(pop))))
  }


  # interfacer::ireturn(
    modelled %>%
    dplyr::mutate(
      incidence.per_capita.0.025 = incidence.0.025 / population * population_unit * timefrac,
      incidence.per_capita.0.5 = incidence.0.5 / population * population_unit * timefrac,
      incidence.per_capita.0.975 = incidence.0.975 / population * population_unit * timefrac,
      incidence.per_capita.fit = incidence.fit - log(population / population_unit) + log(timefrac),
      incidence.per_capita.se.fit = incidence.se.fit - log(population / population_unit) + log(timefrac),
      population_unit = population_unit,
      time_unit = out_unit
    )
  #  i_incidence_per_capita_model
  #)


}


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

# tmp = .loessfn(x=seq(0,5,0.01), y=seq(0,5,0.01)^2)
# tmp(xout=1:4)
.loessfn = function(x,y,window=14) {
  tofit = tibble::tibble(x=x,y=y)
  tmp = stats::loess(y~x, tofit, span = .nn_from_window(window,tofit))
  return(function(xout) {
    yout = stats::predict(tmp, tibble::tibble(x=xout))
    return(unname(yout))
  })
}
