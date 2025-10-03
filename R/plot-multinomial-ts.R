#' Plot a multinomial proportions model
#'
#' A multinomial proportions model will tell you what proportion each class has
#' versus others in the data set. In this case the denominator is the total
#' count across across all classes.
#'
#' @iparam modelled the multinomial count data
#' @param mapping a `ggplot2::aes` mapping. Usually this will be left as the
#'  default
#' @inheritParams geom_events
#' @inheritDotParams geom_events events
#' @param normalise make sure the probabilities add up to one - this can be a
#'  bad idea if you know you may have missing values, on the other hand not all
#'  proportions models are guaranteed to add up to one.
#'
#' @return a ggplot
#' @export
#' @concept vis
#' @examples
#' tmp = ggoutbreak::england_covid_proportion_age_stratified() %>%
#'   dplyr::glimpse()
#'
#' if(interactive()) {
#'   plot_multinomial(tmp, normalise=TRUE)+
#'     ggplot2::scale_fill_viridis_d()
#' }
#'
plot_multinomial = function(
  modelled = i_multinomial_proportion_model,
  ...,
  mapping = ggplot2::aes(fill = class),
  events = i_events,
  normalise = FALSE
) {
  modelled = interfacer::ivalidate(modelled)

  if (normalise) {
    modelled = modelled %>%
      dplyr::ungroup(class) %>%
      dplyr::group_by(time, .add = TRUE) %>%
      dplyr::mutate(proportion.0.5 = proportion.0.5 / sum(proportion.0.5))
  }

  ggplot2::ggplot() +
    .layer(
      ggplot2::GeomArea,
      data = modelled,
      mapping = ggplot2::aes(
        x = as.Date(time),
        y = proportion.0.5 * 100,
        !!!mapping
      ),
      position = "stack",
      ...,
      .default = list(colour = "black", linewidth = 0.1)
    ) +
    geom_events(events, ...) +
    ggplot2::ylab("proportion (%)") +
    ggplot2::xlab(NULL) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::coord_cartesian(expand = FALSE)
}