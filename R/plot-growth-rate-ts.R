# Growth rate timeseries diagram ----

#' Growth rate timeseries diagram
#'
#' @param modelled
#' Either:
#'
#' `r interfacer::idocument(plot_growth_rate.incidence, modelled)`
#'
#' OR:
#'
#' `r interfacer::idocument(plot_growth_rate.proportion, modelled)`
#'
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
#'
#' tmp_pop = ggoutbreak::england_demographics %>%
#'   dplyr::ungroup() %>%
#'   dplyr::summarise(population = sum(population))
#'
#'
#'
#' # If the incidence is normalised by population
#' tmp2 = tmp %>%
#'   poisson_locfit_model() %>%
#'   normalise_incidence(tmp_pop)
#'
#' # Default pdf device doesn't support unicode
#' plot_growth_rate(tmp2,colour="blue")
#'
#' tmp3 = ggoutbreak::england_covid %>%
#'   proportion_locfit_model()
#'
#' # Default pdf device doesn't support unicode
#' plot_growth_rate(tmp3)
#'
plot_growth_rate = function(
    modelled = i_timeseries,
    ...,
    mapping = if (interfacer::is_col_present(modelled, class)) ggplot2::aes(colour = class) else ggplot2::aes(),
    events = i_events
) {
  interfacer::idispatch(
    modelled,
    plot_growth_rate.incidence = i_incidence_rate,
    plot_growth_rate.proportion = i_proportion_rate
  )
}

# internal function for dispatch
plot_growth_rate.incidence = function(
    modelled = i_incidence_rate,
    ...,
    mapping = if (interfacer::is_col_present(modelled, class)) ggplot2::aes(colour = class) else ggplot2::aes(),
    events = i_events
) {

  modelled = interfacer::ivalidate(modelled)

  my = .glimit(modelled$growth.0.5)

  ggplot2::ggplot()+
    geom_events(events,...)+
    ggplot2::geom_hline(yintercept = 0, colour="grey50")+
    .layer(ggplot2::GeomLine,
           data = modelled,
           mapping = ggplot2::aes(x=as.Date(time), y=growth.0.5, !!!mapping),
           ...
    )+
    .layer(ggplot2::GeomRibbon,
           data = modelled,
           mapping = ggplot2::aes(x=as.Date(time), ymin=growth.0.025, ymax=growth.0.975, !!!mapping),
           ...,
           .default=list(colour = NA, alpha=0.2)
    )+
    ggplot2::ylab(sprintf("Growth rate per %s", .fmt_unit(modelled$time)))+
    ggplot2::xlab(NULL)+
    ggplot2::theme(legend.title=ggplot2::element_blank())+
    ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(
      labels = function(x) ifelse(x==0,.pdf_safe("\u00B1\u221E"),sprintf("%.2g",(log(2)/x/.step(modelled$time)))),
      name="Doubling time (days)"))+
    ggplot2::coord_cartesian(ylim = c(-my,my))

}

.glimit = function(y) {
  ceiling(max(abs(y),na.rm=TRUE)*20)/20
}

# internal function for dispatch
plot_growth_rate.proportion = function(
    modelled = i_proportion_rate,
    ...,
    mapping = if (interfacer::is_col_present(modelled, class)) ggplot2::aes(colour = class) else ggplot2::aes(),
    events = i_events
) {

  modelled = interfacer::ivalidate(modelled)

  my = .glimit(modelled$relative.growth.0.5)

  ggplot2::ggplot()+
    geom_events(events,...)+
    ggplot2::geom_hline(yintercept = 0, colour="grey50")+
    .layer(ggplot2::GeomLine,
           data = modelled,
           mapping = ggplot2::aes(x=as.Date(time), y=relative.growth.0.5, !!!mapping),
           ...
    )+
    .layer(ggplot2::GeomRibbon,
           data = modelled,
           mapping = ggplot2::aes(x=as.Date(time), ymin=relative.growth.0.025, ymax=relative.growth.0.975, !!!mapping),
           ...,
           .default = list(colour = NA, alpha=0.2)
    )+
    ggplot2::ylab(sprintf("Relative growth rate per %s", .fmt_unit(modelled$time)))+
    ggplot2::xlab(NULL)+
    ggplot2::theme(legend.title=ggplot2::element_blank())+
    ggplot2::coord_cartesian(ylim = c(-my,my))

  # growth rate limits do not apply for relative

}


