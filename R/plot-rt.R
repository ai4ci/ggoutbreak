# Reproduction numner timeseries diagram ----

#' Reproduction number timeseries diagram
#'
#' @iparam modelled the modelled Rt estimate
#' @param mapping a `ggplot2::aes` mapping. Most importantly setting the `colour`
#'   to something if there are multiple incidence time series in the plot
#' @inheritParams geom_events
#' @inheritDotParams geom_events events
#'
#' @return a ggplot timeseries
#' @export
#' @concept vis
#' @examples
#' # example code
#' tmp = ggoutbreak::england_covid %>%
#'   time_aggregate(count=sum(count))
#' if (FALSE) {
#'
#'   tmp2 = tmp %>%
#'     poisson_locfit_model() %>%
#'     rt_from_growth_rate()
#'
#'   # comparing RT from growth rates with England consensus Rt:
#'   plot_rt(tmp2,colour="blue")+
#'     geom_errorbar(data=england_consensus_rt, mapping=aes(x=date-21,ymin=low,ymax=high),colour="red")
#'
#' }
plot_rt = function(
    modelled = i_reproduction_number,
    ...,
    mapping = if (interfacer::is_col_present(modelled, class)) ggplot2::aes(colour = class) else ggplot2::aes(),
    events = i_events
) {

  modelled = interfacer::ivalidate(modelled)

  ggplot2::ggplot()+
    geom_events(events,...)+
    ggplot2::geom_hline(yintercept = 1, colour="grey50")+
    .layer(ggplot2::GeomLine,
           data = modelled,
           mapping = ggplot2::aes(x=as.Date(time), y=rt.0.5, !!!mapping),
           ...
    )+
    .layer(ggplot2::GeomRibbon,
           data = modelled,
           mapping = ggplot2::aes(x=as.Date(time), ymin=rt.0.025, ymax=rt.0.975, !!!mapping),
           ...,
           .default=list(colour = NA, alpha=0.2)
    )+
    ggplot2::ylab("Effective Rt")+
    ggplot2::xlab(NULL)+
    ggplot2::theme(legend.title=ggplot2::element_blank())+
    ggplot2::coord_cartesian(ylim = c(0.5,2.5))

}

