#' Calculate a normalised risk ratio from proportions
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This assumes that for example, case distribution proportions are stratified
#' by a population grouping, e.g. geography or age, and we have estimates of the
#' size of that population during that time period. Normalising by population
#' proportion allows us to compare the relative risk of the outcome in groups,
#' compared to the expected population risk if the outcome is evenly distributed
#' across the population. There may be other proportions other than population
#' fractions that may be useful to compare. At the moment this does not handle
#' any uncertainty.
#'
#' @iparam base The baseline data must be grouped in the same way as `modelled`.
#'   It may be a time series but does not have to be.
#' @iparam modelled Model output from something like [proportion_locfit_model()]
#' @param ... not used
#'
#' @return a dataframe with relative risk / risk ratio columns.
#'   `r i_risk_ratio_model`
#' @export
#' @concept models
#'
#' @examples
#'
#' tmp = ggoutbreak::england_covid_proportion_age_stratified()  %>%
#'   ggoutbreak::infer_risk_ratio(ggoutbreak::england_demographics) %>%
#'   dplyr::glimpse()
#'
#' if(interactive()) {
#'   plot_growth_phase(tmp,duration = 14*4)+
#'   ggplot2::scale_colour_viridis_d()+
#'   ggplot2::coord_cartesian(xlim=c(-0.25,0.25),ylim=c(0.02,20))
#' }
#'
infer_risk_ratio = function(
  modelled = i_proportion_model,
  base = i_baseline_proportion_data,
  ...
) {
  base = interfacer::ivalidate(base, ...)
  modelled = interfacer::ivalidate(modelled, ...)

  modelled = modelled %>% .impute_or_join(base, baseline_proportion)

  # interfacer::ireturn(
  modelled %>%
    dplyr::mutate(
      risk_ratio.0.025 = proportion.0.025 / baseline_proportion,
      risk_ratio.0.5 = proportion.0.5 / baseline_proportion,
      risk_ratio.0.975 = proportion.0.975 / baseline_proportion,
      # risk_ratio.fit = proportion.fit - log(baseline_proportion),
      # risk_ratio.se.fit = proportion.fit - log(baseline_proportion)
    )
  #  i_risk_ratio_model
  #)
}


#' Calculate a risk ratio from incidence
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This enables incidence rates are able to be compared to a baseline figure for
#' incidence. The baseline could come for example from a population average or
#' average incidence over time. The output is an incidence rate ratio. The
#' `incidence_baseline` column is a rate of events per unit time. The time unit
#' is expected to be the same as that of the date in `modelled` and this is not
#' checked.
#'
#' @iparam base The baseline data must be grouped in the same way as `modelled`.
#'   It may be a time series but does not have to be. See the example and note
#'   this may change in the future.
#' @iparam modelled Model output from something like [poisson_locfit_model()].
#'   It really makes sense if this is a grouped model.
#' @param ... not used
#'
#' @return a dataframe with incidence rate ratios for each of the classes in modelled.
#'   `r i_rate_ratio_model`
#' @export
#' @concept models
#'
#' @examples
#'
#' baseline = ggoutbreak::england_covid_poisson %>%
#'   dplyr::mutate(baseline_incidence = incidence.0.5)
#'
#'
#' tmp = ggoutbreak::england_covid_poisson_age_stratified() %>%
#'   ggoutbreak::infer_rate_ratio(baseline) %>%
#'   dplyr::glimpse()
#'
#'
infer_rate_ratio = function(
  modelled = i_incidence_model,
  base = i_baseline_incidence_data,
  ...
) {
  #TODO: no plotting method defined for these rate ratios.
  #TODO: the growth rate returned here is absolute not relative
  #TODO: We could have done this with proper confidence as the ratio of
  # log normals if we had them.
  # This is almost a family of related functions depending on the input data
  # base would then be input as an incidence model.
  #TODO: infer_relative_growth as another function which takes 2 growth rates
  # and returns the relative growth rate.
  #TODO: Plotting for incidence rate ratio on its own and
  # with relative growth rate in a growth phase diagram.

  base = interfacer::ivalidate(base, ...)
  modelled = interfacer::ivalidate(modelled, ...)

  modelled = modelled %>% .impute_or_join(base, baseline_incidence)

  # interfacer::ireturn(
  modelled %>%
    dplyr::mutate(
      rate_ratio.0.025 = incidence.0.025 / baseline_incidence,
      rate_ratio.0.5 = incidence.0.5 / baseline_incidence,
      rate_ratio.0.975 = incidence.0.975 / baseline_incidence,
      # risk_ratio.fit = proportion.fit - log(baseline_proportion),
      # risk_ratio.se.fit = proportion.fit - log(baseline_proportion)
    )
  #  i_risk_ratio_model
  #)
}