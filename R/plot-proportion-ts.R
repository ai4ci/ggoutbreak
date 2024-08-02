#' Plot a proportions timeseries
#'
#' @iparam raw Raw count data
#' @iparam modelled Proportion model estimates
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
#' tmp = ggoutbreak::england_covid %>%
#'   ggoutbreak::proportion_locfit_model(window=21) %>%
#'   dplyr::glimpse()
#'
#' plot_proportion(tmp)+
#'   ggplot2::scale_fill_viridis_d(aesthetics = c("fill","colour"))
plot_proportion = function(
    modelled = i_proportion_model,
    raw = i_proportion_data,
    ...,
    mapping = if (interfacer::is_col_present(modelled, class)) ggplot2::aes(colour = class) else ggplot2::aes(),
    events = i_events
) {

  if (!interfacer::is.iface(raw)) {
    raw = interfacer::ivalidate(raw)
    if (interfacer::is.iface(modelled)) modelled = raw %>% dplyr::group_modify(proportion_locfit_model, ...)
    plot_points = TRUE
  } else {
    plot_points = FALSE
  }
  modelled = interfacer::ivalidate(modelled)

  ggplot2::ggplot()+
    geom_events(events, ...)+
    .layer(ggplot2::GeomLine,
           data = modelled,
           mapping = ggplot2::aes(x=as.Date(time), y=proportion.0.5*100, !!!mapping),
           ...)+
    .layer(ggplot2::GeomRibbon,
           data = modelled,
           mapping = ggplot2::aes(x=as.Date(time), ymin=proportion.0.025*100, ymax=proportion.0.975*100, !!!mapping),
           ...,
           .default = list(colour = NA, alpha=0.2)
    )+
    {
      if (plot_points) {
        .layer(ggplot2::GeomPoint,
               data = raw, mapping=ggplot2::aes(x=as.Date(time), y=count/denom*100, !!!mapping), ...)
      } else {NULL}
    }+
    ggplot2::ylab("Proportion (%)")+
    ggplot2::xlab(NULL)+
    ggplot2::theme(legend.title=ggplot2::element_blank())


}

#TODO: risk_ratio
# risk_ratio data for points
#
