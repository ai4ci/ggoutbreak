# Primary data timeseries diagram ----

#' Plot an incidence timeseries
#'
#' @iparam raw The raw count data
#' @param modelled An optional estimate of the incidence time series. If `modelled` is
#'   missing then it is estimated from `raw` using a `poisson_locfit_model`.
#'   In this case parameters `window` and `deg` may be supplied to control the
#'   fit.
#'
#'   `r interfacer::idocument(plot_incidence, modelled)`
#'
#'   `modelled` can also be the output from `normalise_incidence` in which case
#'   the plot uses the per capita rates calculated by that function
#' @param mapping a `ggplot2::aes` mapping. Most importantly setting the `colour`
#'   to something if there are multiple incidence timeseries in the plot
#' @inheritParams geom_events
#' @inheritDotParams geom_events events
#' @inheritDotParams poisson_locfit_model window deg frequency
#'
#' @return a ggplot object
#' @export
#' @concept vis
#' @examples
#' # example code
#'
#' tmp = ggoutbreak::england_covid %>%
#'   time_aggregate(count=sum(count)) %>%
#'   normalise_count(56489700, population_unit=1000, normalise_time="1 year")
#'
#' tmp2 = tmp %>% poisson_locfit_model()
#'
#' plot_incidence(tmp2,tmp,colour="blue",size=0.25)
#'
plot_incidence = function(
    modelled = i_incidence_model,
    raw = i_incidence_data,
    ...,
    mapping = .check_for_aes(modelled,...),
    events = i_events
) {

  if (interfacer::is.iface(modelled)) modelled = raw %>% poisson_locfit_model(...)

  interfacer::idispatch(modelled,
    plot_incidence.per_capita = i_incidence_per_capita_model,
    plot_incidence.default = i_incidence_model
  )

}


plot_incidence.default = function(
  modelled = i_incidence_model,
  raw = i_incidence_data,
  ...,
  mapping = .check_for_aes(modelled,...),
  events = i_events
) {

  if (!interfacer::is.iface(raw)) {
    raw = interfacer::ivalidate(raw)
    timefrac = attr(modelled$time,"unit") / attr(raw$time,"unit")
    plot_points = TRUE
  } else {
    plot_points = FALSE
  }

  ggplot2::ggplot()+
    geom_events(events, ...)+
    .layer(
      geom = ggplot2::GeomLine,
      data = modelled,
      mapping = ggplot2::aes(x=as.Date(time), y=incidence.0.5, !!!mapping),
      ...
    )+
    .layer(
      geom = ggplot2::GeomRibbon,
      data = modelled,
      mapping = ggplot2::aes(x=as.Date(time), ymin=incidence.0.025, ymax=incidence.0.975, !!!.fill_col(mapping)),
      ...,
      .default=list(colour = NA, alpha=0.2)
    )+
    {
      if (plot_points) {
        if (timefrac==1) {
          .layer(
            ggplot2::GeomPoint,
            data = raw,
            mapping=ggplot2::aes(x=as.Date(time), y=count*timefrac, !!!mapping),
            ...
          )
        } else {
          .layer(
            ggplot2::GeomSegment,
            data = raw,
            mapping=ggplot2::aes(x=as.Date(time-0.5),xend=as.Date(time+0.5), y=count*timefrac,yend=count*timefrac, !!!mapping),
            ...
          )
        }
      } else {NULL}
    }+
    ggplot2::scale_y_continuous(breaks = .integer_breaks())+
    ggplot2::ylab(sprintf("cases per %s", .fmt_unit(modelled$time)))+
    ggplot2::xlab(NULL)+
    ggplot2::theme(legend.title=ggplot2::element_blank())

}

plot_incidence.per_capita = function(
  modelled = i_incidence_per_capita_model,
  raw = i_incidence_per_capita_data,
  ...,
  mapping = .check_for_aes(modelled,...),
  events = i_events
) {

  population_unit = unique(modelled$population_unit)
  time_unit = modelled$time_unit[[1]] # e.g. modelled nornalised time unit is per year
  # unique() does not work on lubridate::periods
  # TODO assume no-one is obtuse enough to have different durations in same dataframe

  if (!interfacer::is.iface(raw)) {

    raw = interfacer::ivalidate(raw)

    raw_time_unit = raw$time_unit[[1]]
    timefrac = time_unit/raw_time_unit
    raw_population_unit = unique(raw$population_unit)
    popfrac = population_unit/raw_population_unit

    # make sure the raw data is on the same temporal and population unit as the modelled data.
    raw = raw %>% dplyr::mutate(count.per_capita = count.per_capita*timefrac*popfrac)
    plot_points = TRUE
  } else {
    plot_points = FALSE
  }



  # modelled = interfacer::ivalidate(modelled)

  ggplot2::ggplot()+
    geom_events(events, ...)+
    .layer(
      geom = ggplot2::GeomLine,
      data = modelled,
      mapping = ggplot2::aes(x=as.Date(time), y=incidence.per_capita.0.5, !!!mapping),
      ...
    )+
    .layer(
      geom = ggplot2::GeomRibbon,
      data = modelled,
      mapping = ggplot2::aes(x=as.Date(time), ymin=incidence.per_capita.0.025, ymax=incidence.per_capita.0.975, !!!.fill_col(mapping)),
      ...,
      .default=list(colour = NA, alpha=0.2)
    )+
    {
      if (plot_points) {
        .layer(
          ggplot2::GeomPoint,
          data = raw,
          mapping=ggplot2::aes(x=as.Date(time), y=count.per_capita, !!!mapping),
          ...,
          .default=list(size=1)
        )
      } else {NULL}
    }+
    ggplot2::ylab(sprintf("cases per %s per %s", .fmt_pop(population_unit), .fmt_unit(time_unit)))+
    ggplot2::xlab(NULL)+
    ggplot2::theme(legend.title=ggplot2::element_blank())

}


