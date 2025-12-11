# Primary data timeseries diagram ----

#' Plot a raw case count timeseries
#'
#' @iparam raw The raw count data, or the raw count data normalised by population (
#'  see [normalise_count()])
#' @param mapping a `ggplot2::aes` mapping. Most importantly setting the `colour`
#'   to something if there are multiple incidence timeseries in the plot
#' @inheritParams geom_events
#' @inheritDotParams geom_events events
#'
#' @return a ggplot object
#' @export
#' @concept vis
#' @examples
#' # example code
#'
#' tmp = example_england_covid_by_age() %>%
#'   time_aggregate(count=sum(count)) %>%
#'   normalise_count(pop=56489700, population_unit=1000, normalise_time=TRUE)
#'
#' # normalised by England population (56489700 people)
#'
#' if(interactive()) {
#'   plot_counts(tmp, colour="blue",size=0.25)
#' }
#'
plot_counts = function(
  raw,
  ...,
  mapping = .check_for_aes(raw, ...),
  events = i_events
) {
  interfacer::idispatch(
    raw,
    plot_counts.per_capita = i_incidence_per_capita_normalised,
    plot_counts.default = i_incidence_data
  )
}


plot_counts.default = function(
  raw = i_incidence_data,
  ...,
  mapping = .check_for_aes(raw, ...),
  events = i_events
) {
  # raw = interfacer::ivalidate(raw)
  # unit = attr(raw$time,"unit")
  # days = as.integer(unit)/(24*60*60)

  ggplot2::ggplot(
    data = raw,
    mapping = ggplot2::aes(
      x = .x_axis(time, ...),
      y = count,
      !!!mapping
    )
  ) +
    geom_events(events, ..., unit = raw$time) +
    .layer(
      # ggplot2::GeomSegment,
      # data = raw,
      # mapping=ggplot2::aes(x=as.Date(time-0.5),xend=as.Date(time+0.5), y=count,yend=count, !!!mapping),
      ggplot2::GeomPoint,
      data = raw,
      mapping = ggplot2::aes(x = .x_axis(time, ...), y = count, !!!mapping),
      ...
    ) +
    suppressWarnings(ggplot2::scale_y_continuous(breaks = integer_breaks())) +
    ggplot2::ylab(sprintf("cases per %s", .fmt_unit(raw$time))) +
    ggplot2::xlab(NULL) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
}

plot_counts.per_capita = function(
  raw = i_incidence_per_capita_normalised,
  ...,
  mapping = .check_for_aes(raw, ...),
  events = i_events
) {
  population_unit = unique(raw$population_unit)
  time_unit = raw$time_unit[[1]] # e.g. raw normalised time unit is per year
  # unique() does not work on lubridate::periods
  #TODO assume no-one is obtuse enough to have different durations in same dataframe

  timefrac = time_unit / .get_meta(raw$time)$unit # original time unit is per week => we want to *52/7
  raw = interfacer::ivalidate(raw)
  raw = raw %>%
    dplyr::mutate(
      count.per_capita = count / population * population_unit * timefrac
    )

  ggplot2::ggplot(
    data = raw,
    mapping = ggplot2::aes(
      x = .x_axis(time, ...),
      y = count.per_capita,
      !!!mapping
    ),
  ) +
    geom_events(events, ..., unit = raw$time) +
    .layer(
      ggplot2::GeomPoint,
      data = raw,
      mapping = ggplot2::aes(
        x = .x_axis(time, ...),
        y = count.per_capita,
        !!!mapping
      ),
      ...,
      .default = list(size = 1)
    ) +
    ggplot2::ylab(sprintf(
      "cases per %s per %s",
      .fmt_pop(population_unit),
      .fmt_unit(time_unit)
    )) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
}

#' Plot a raw case count proportion timeseries
#'
#' @iparam raw The raw count and denominator data
#' @param mapping a `ggplot2::aes` mapping. Most importantly setting the `colour`
#'   to something if there are multiple count timeseries in the data
#' @inheritParams geom_events
#' @inheritDotParams geom_events events
#'
#' @return a ggplot object
#' @export
#' @concept vis
#' @examples
#'
#' tmp = example_england_covid_by_age() %>%
#'   dplyr::filter(class %in% c("50_54","80_84"))
#'
#' if(interactive()) {
#'   plot_proportions_data(tmp, mapping= ggplot2::aes(colour=class))+ggplot2::geom_line()
#' }
#'
plot_proportions_data = function(
  raw = i_proportion_data,
  ...,
  mapping = .check_for_aes(raw, ...),
  events = i_events
) {
  raw = interfacer::ivalidate(raw)
  raw = raw %>% dplyr::mutate(count.percent = count / denom * 100)

  ggplot2::ggplot(
    data = raw,
    mapping = ggplot2::aes(
      x = .x_axis(time, ...),
      y = count.percent,
      !!!mapping
    )
  ) +
    geom_events(events, ..., unit = raw$time) +
    .layer(
      ggplot2::GeomPoint,
      data = raw,
      mapping = ggplot2::aes(
        x = .x_axis(time, ...),
        y = count.percent,
        !!!mapping
      ),
      ...,
      .default = list(size = 1)
    ) +
    ggplot2::ylab("proportion (%)") +
    ggplot2::xlab(NULL) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
}


## Plot linelist

#' Plot a line-list of cases as a histogram
#'
#' @iparam raw The raw case data either as a summarised count or as a line-list
#' @param mapping a `ggplot2::aes` mapping. Most importantly setting the `fill`
#'   to something if there are multiple types of event in the plot. If a `class`
#'   column is present the mapping will default to using this.
#' @inheritParams geom_events
#' @inheritDotParams geom_events events
#'
#' @return a ggplot object
#' @export
#' @concept vis
#' @examples
#'
#' with_defaults("2025-01-01" ,"1 day", {
#'   tmp = dplyr::tibble(
#'     time = as.time_period( rexpgrowth(100,0.05,40)),
#'     class = rep(c("one","two","three"), length.out=100)
#'   )
#' })
#'
#' if(interactive()) {
#'   plot_cases(tmp, mapping=ggplot2::aes(fill = class),linewidth=0.1,colour="white")
#' }
plot_cases = function(
  raw,
  ...,
  mapping = .check_for_aes(raw, ..., class_aes = "fill"),
  events = i_events
) {
  interfacer::idispatch(
    raw,
    plot_cases.summarised = i_incidence_data,
    plot_cases.default = i_timestamped
  )
}

plot_cases.summarised = function(
  raw = i_incidence_data,
  ...,
  mapping = .check_for_aes(raw, ..., class_aes = "fill"),
  events = i_events
) {
  events = interfacer::ivalidate(events)
  raw = raw %>%
    dplyr::mutate(
      count = purrr::map(count, ~ rep(1, .x))
    ) %>%
    tidyr::unnest(count)

  plot_cases.default(raw, ..., mapping = mapping, events = force(events))
}

plot_cases.default = function(
  raw = i_timestamped,
  ...,
  mapping = .check_for_aes(raw, ..., class_aes = "fill"),
  events = i_events,
  individual = nrow(raw) < 100
) {
  events = interfacer::ivalidate(events)
  raw = raw %>%
    dplyr::mutate(time = floor(time)) %>%
    dplyr::arrange(time, .by_group = TRUE) %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(y = dplyr::row_number())
  unit = attr(raw$time, "unit")
  days = as.integer(unit) / (24 * 60 * 60)

  # TODO: support faceting in plot_cases with a stat:
  # https://ggplot2-book.org/extensions.html#sec-new-stats

  ggplot2::ggplot(
    data = raw,
    mapping = ggplot2::aes(x = .x_axis(time, ...), y = 1, !!!mapping)
  ) +
    geom_events(events, ..., unit = raw$time) +
    .layer(
      ggplot2::GeomRect,
      data = raw,
      mapping = ggplot2::aes(
        xmin = .x_axis(time - 0.5, ...),
        xmax = .x_axis(time + 0.5, ...),
        ymin = y - 1,
        ymax = y,
        !!!mapping
      ),
      .default = {
        if (individual) {
          list(colour = "white", linewidth = 0.25)
        } else {
          list()
        }
      },
      ...
    ) +
    ggplot2::ylab(sprintf("cases per %s", .fmt_unit(raw$time))) +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    suppressWarnings(ggplot2::scale_y_continuous(breaks = integer_breaks())) +
    {
      if (individual) ggplot2::coord_fixed() else NULL
    }
}
