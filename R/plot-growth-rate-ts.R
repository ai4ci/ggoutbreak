# Growth rate timeseries diagram ----

#' Growth rate timeseries diagram
#'
#' @iparam modelled a growth rate dataframe.
#' @param mapping a `ggplot2::aes` mapping. Most importantly setting the `colour`
#'   to something if there are multiple incidence time series in the plot
#' @inheritParams geom_events
#' @inheritDotParams geom_events events
#' @inheritDotParams geom_truth
#'
#' @return a ggplot
#' @export
#' @concept vis
#' @examples
#'
#' data = example_poisson_rt_2class()
#' tmp2 = data %>% poisson_locfit_model()
#'
#' if(interactive()) {
#'   plot_growth_rate(tmp2)
#' }
#'
plot_growth_rate = function(
  modelled,
  ...,
  mapping = .check_for_aes(modelled, ...),
  events = i_events
) {
  interfacer::idispatch(
    modelled,
    plot_growth_rate.default = i_growth_rate,
    plot_growth_rate.relative = i_relative_growth_rate
  )
}

# internal function for dispatch
plot_growth_rate.default = function(
  modelled = i_incidence_rate,
  ...,
  mapping = .check_for_aes(modelled, ...),
  events = i_events
) {
  # modelled = interfacer::ivalidate(modelled)

  my = .glimit(modelled$growth.0.5)

  ggplot2::ggplot() +
    geom_events(events, ..., unit = modelled$time) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    .layer(
      ggplot2::GeomLine,
      data = modelled,
      mapping = ggplot2::aes(
        x = .x_axis(time, ...),
        y = growth.0.5,
        !!!mapping
      ),
      ...
    ) +
    .layer(
      ggplot2::GeomRibbon,
      data = modelled,
      mapping = ggplot2::aes(
        x = .x_axis(time, ...),
        ymin = growth.0.025,
        ymax = growth.0.975,
        !!!mapping
      ),
      ...,
      .default = list(colour = NA, alpha = 0.2)
    ) +
    ggplot2::ylab(sprintf("growth rate per %s", .fmt_unit(modelled$time))) +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::dup_axis(
        labels = function(x) {
          ifelse(
            x == 0,
            .pdf_safe("\u00B1\u221E"),
            sprintf("%.2g", (log(2) / x / .step(modelled$time)))
          )
        },
        name = "doubling time (days)"
      )
    ) +
    .gdefaultcoords(modelled$growth.0.5) +
    geom_truth(...)
}

.glimit = function(y, res = 1 / 20) {
  maxy = quantile(abs(y), 0.99, na.rm = TRUE) * 1.1
  ceiling(maxy / res) * res
}

.gdefaultcoords = function(y, res = 1 / 20) {
  my = .glimit(y, res)
  if (!any(y < 0, na.rm = TRUE)) {
    my2 = 0
  } else {
    my2 = -my
  }
  ggplot2::coord_cartesian(ylim = c(my2, my))
}

# internal function for dispatch
plot_growth_rate.relative = function(
  modelled = i_relative_growth_rate,
  ...,
  mapping = .check_for_aes(modelled, ...),
  events = i_events
) {
  # modelled = interfacer::ivalidate(modelled)

  my = .glimit(modelled$relative.growth.0.5)

  ggplot2::ggplot() +
    geom_events(events, ..., unit = modelled$time) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    .layer(
      ggplot2::GeomLine,
      data = modelled,
      mapping = ggplot2::aes(
        x = .x_axis(time, ...),
        y = relative.growth.0.5,
        !!!mapping
      ),
      ...
    ) +
    .layer(
      ggplot2::GeomRibbon,
      data = modelled,
      mapping = ggplot2::aes(
        x = .x_axis(time, ...),
        ymin = relative.growth.0.025,
        ymax = relative.growth.0.975,
        !!!mapping
      ),
      ...,
      .default = list(colour = NA, alpha = 0.2)
    ) +
    ggplot2::ylab(sprintf(
      "relative growth rate per %s",
      .fmt_unit(modelled$time)
    )) +
    ggplot2::xlab(NULL) +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    .gdefaultcoords(modelled$relative.growth.0.5) +
    geom_truth(...)

  # growth rate limits do not apply for relative
}
