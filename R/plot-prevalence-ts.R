#' Plot a timeseries of disease prevalence
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @iparam raw Raw proportion data
#' @iparam modelled Prevalence estimates
#' @param mapping a `ggplot2::aes` mapping. Most importantly setting the `colour`
#'   to something if there are multiple incidence timeseries in the plot
#' @inheritParams geom_events
#' @inheritDotParams geom_events events
#' @inheritDotParams proportion_locfit_model window deg frequency
#'
#' @return a ggplot object
#' @export
#' @concept vis
#' @examples
#'
#' if(interactive()) {
#'
#'   plot_prevalence(
#'     ukc19::ons_infection_survey %>%
#'       dplyr::mutate(time = as.time_period(date,"1 day")),
#'     mapping = ggplot2::aes(colour=name))
#' }
plot_prevalence = function(
  modelled = i_prevalence_model,
  raw = i_proportion_data,
  ...,
  mapping = .check_for_aes(modelled, ...),
  events = i_events
) {
  modelled = interfacer::ivalidate(modelled)
  prop_model = modelled %>%
    dplyr::rename_with(
      ~ gsub("prevalence.", "proportion.", .x, fixed = TRUE),
      .cols = dplyr::starts_with("prevalence.")
    ) %>%
    dplyr::mutate(
      proportion.fit = NA,
      proportion.se.fit = NA
    )

  plot = plot_proportion(
    prop_model,
    raw = raw,
    ...,
    mapping = mapping,
    events = events
  )

  plot +
    ggplot2::ylab("prevalence (%)")
}
