# Reproduction numner timeseries diagram ----

#' Reproduction number timeseries diagram
#'
#' @iparam modelled the modelled Rt estimate
#' @param mapping a `ggplot2::aes` mapping. Most importantly setting the `colour`
#'   to something if there are multiple incidence time series in the plot
#' @inheritParams geom_events
#' @inheritDotParams geom_events events
#' @inheritDotParams geom_truth
#'
#' @return a ggplot timeseries
#' @export
#' @concept vis
#' @examples
#' # example code
#' if (interactive()) {
#'
#' tmp2 = example_poisson_locfit() %>%
#'   dplyr::filter(as.Date(time) >= "2021-01-01" & as.Date(time) < "2022-01-01") %>%
#'   rt_from_incidence(ip = example_ganyani_ip())
#'
#' # comparing RT from growth rates with England consensus Rt
#' # (N.B. offset by 14 days to align with estimates):
#'
#' plot_rt(tmp2,colour="blue")+
#'   ggplot2::geom_errorbar(
#'     data= ukc19::spim_consensus %>%
#'       dplyr::filter(date-14 >= "2021-01-01" & date-14 < "2022-01-01"),
#'     mapping=ggplot2::aes(x=date-14,ymin=rt.low,ymax=rt.high),
#'     colour="red")
#'
#' }
plot_rt = function(
  modelled = i_reproduction_number,
  ...,
  mapping = .check_for_aes(modelled, ...),
  events = i_events
) {
  modelled = interfacer::ivalidate(modelled)

  ul = min(c(
    max(modelled$`rt.0.975`, na.rm = TRUE),
    1.25 * max(modelled$`rt.0.5`, na.rm = TRUE)
  ))

  ggplot2::ggplot() +
    geom_events(events, ..., unit = modelled$time) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey50") +
    .layer(
      ggplot2::GeomLine,
      data = modelled,
      mapping = ggplot2::aes(x = .x_axis(time, ...), y = rt.0.5, !!!mapping),
      ...
    ) +
    .layer(
      ggplot2::GeomRibbon,
      data = modelled,
      mapping = ggplot2::aes(
        x = .x_axis(time, ...),
        ymin = rt.0.025,
        ymax = rt.0.975,
        !!!mapping
      ),
      ...,
      .default = list(colour = NA, alpha = 0.2)
    ) +
    ggplot2::ylab("effective Rt") +
    # ggplot2::xlab(NULL) +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(ylim = c(NA, ul)) +
    geom_truth(...)
}
