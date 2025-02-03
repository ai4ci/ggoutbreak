

#' Calculate a normalised risk ratio from proportions
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
#'
#'
#' tmp = ggoutbreak::england_covid %>%
#'   ggoutbreak::proportion_locfit_model(window=21) %>%
#'   ggoutbreak::normalise_proportion(ggoutbreak::england_demographics) %>%
#'   dplyr::glimpse()
#'
#' plot_growth_phase(tmp)
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
#' This enables incidence rates are able to be compared to a baseline figure for
#' incidence. The baseline could come for example from a population average or
#' average incidence over time. The output is an incidence rate ratio. The
#' `incidence_baseline` column is a rate of events per unit time. The time unit
#' is expected to be the same as that of the date in `modelled` and this is not
#' checked.
#'
#' @iparam base The baseline data must be grouped in the same way as `modelled`.
#'   It may be a time series but does not have to be.
#' @iparam modelled Model output from something like [poisson_locfit_model()]
#' @param ... not used
#'
#' @return a dataframe with incidence rates per unit capita.
#'   `r i_rate_ratio_model`
#' @export
#' @concept models
#'
#' @examples
#'
#'
#'
#' tmp = ggoutbreak::england_covid %>%
#'   ggoutbreak::proportion_locfit_model(window=21) %>%
#'   ggoutbreak::normalise_proportion(ggoutbreak::england_demographics) %>%
#'   dplyr::glimpse()
#'
#' plot_growth_phase(tmp)
#'
infer_rate_ratio = function(
    modelled = i_incidence_model,
    base = i_baseline_incidence_data,
    ...
) {

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
