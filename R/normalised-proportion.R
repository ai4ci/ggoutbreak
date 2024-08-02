

#' Calculate a normalised risk ration from proportions
#'
#' This assumes case distribution proportions are stratified by a population grouping, e.g.
#' geography or age, and we have estimates of the size of that population during
#' that time period. Normalising by population proportion allows us to compare groups.
#'
#' @iparam base The baseline data must be grouped in the same way as `modelled`.
#' @iparam modelled Model output from processing the `raw` dataframe with something like
#'   `proportion_locfit_model`
#' @param ... not used
#'
#' @return a dataframe with incidence rates per unit capita.
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
normalise_proportion = function(
    modelled = i_proportion_model,
    base = i_baseline_proportion_data,
    ...
) {

  base = interfacer::ivalidate(base, ...)
  modelled = interfacer::ivalidate(modelled, ...)
  if (!all(dplyr::group_vars(base) %in% dplyr::group_vars(modelled)))
    stop(
      "different column groupings in `modelled` and `base` parameters.\n",
      "`modelled` must be compatible from the `base` data"
    )

  if (interfacer::itest(base, i_timeseries)) {

    # pop is given as a timeseries we need to impute values
    adj = base %>% dplyr::reframe(
      population_fn = list(.loessfn(x = time, y=baseline_proportion))
    )
    modelled = modelled %>%
      dplyr::group_modify(function(d,g,...) {

        pop_fn = adj %>%
          dplyr::semi_join(g, by=colnames(g)) %>%
          dplyr::pull(population_fn) %>%
          `[[`(1)

        d %>% dplyr::mutate(baseline_proportion = pop_fn(time))
      })
  } else {

    # pop is static.
    modelled = modelled %>%
      dplyr::inner_join(base, by=dplyr::group_vars(base)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(dplyr::group_vars(base))))
  }


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

