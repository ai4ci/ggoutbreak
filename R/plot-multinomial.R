#' Plot a multinomial proportions mode
#'
#' @iparam modelled the multinomial count data
#' @param mapping a `ggplot2::aes` mapping. Most importantly setting the `colour`
#'   to something if there are multiple incidence timeseries in the plot
#' @inheritParams geom_events
#' @inheritDotParams geom_events events
#' @param normalise make sure the probabilities add up to one - this can be a
#'  bad idea if you know you may have missing values.
#'
#' @return a ggplot
#' @export
#' @concept vis
#' @examples
#' tmp = ggoutbreak::england_covid %>%
#'   ggoutbreak::proportion_locfit_model(window=21) %>%
#'   dplyr::glimpse()
#'
#' plot_multinomial(tmp, normalise=TRUE)+
#'   ggplot2::scale_fill_viridis_d()
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
    modelled = modelled %>% dplyr::ungroup(class) %>% dplyr::group_by(time,.add=TRUE) %>%
      dplyr::mutate(proportion.0.5 = proportion.0.5/sum(proportion.0.5))
  }

  ggplot2::ggplot()+
    .layer(ggplot2::GeomArea,
           data = modelled,
           mapping = ggplot2::aes(x=as.Date(time), y=proportion.0.5*100, !!!mapping),
           position = "stack",
           ...,
           .default = list(colour="black", linewidth=0.1)
    )+
    geom_events(events, ...)+
    ggplot2::ylab("Proportion (%)")+
    ggplot2::xlab(NULL)+
    ggplot2::theme(
      legend.title=ggplot2::element_blank()
    )+
    ggplot2::coord_cartesian(expand=FALSE)
}

