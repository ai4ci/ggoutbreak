# Phase diagram ----

#' Plot an incidence or proportion vs. growth phase diagram
#'
#' @param modelled
#' Either:
#'
#' `r interfacer::idocument(plot_growth_rate.incidence, modelled)`
#'
#' OR:
#'
#' `r interfacer::idocument(plot_growth_rate.proportion, modelled)`
#' @param timepoints timepoints (as `Date` or `time_period` vector) of dates to
#'   plot phase diagrams. If multiple this will result in a sequence of plots
#'   as facets. If `NULL` (the default) it will be the last time point in the series
#' @param duration the length of the growth rate phase trail
#' @param interval the length of time between markers on the phase plot
#' @param mapping a `ggplot2::aes()` mapping
#' @param cis logical; should the phases be marked with confidence intervals?
#' @inheritParams geom_events
#' @inheritDotParams geom_events events
#'
#' @return a ggplot
#' @export
#' @concept vis
#' @examples
#'
#' tmp = ggoutbreak::england_covid %>%
#'   time_aggregate(count=sum(count))
#'
#' tmp_pop = ggoutbreak::england_demographics %>%
#'   dplyr::ungroup() %>%
#'   dplyr::summarise(population = sum(population))
#'
#' # If the incidence is normalised by population
#' tmp2 = tmp %>%
#'   poisson_locfit_model() %>%
#'   normalise_incidence(tmp_pop)
#'
#' timepoints = as.Date(c("Lockdown 1" = "2020-03-30", "Lockdown 2" = "2020-12-31"))
#'
#' plot_growth_phase(tmp2, timepoints, duration=108)
plot_growth_phase = function(
    modelled = i_timestamped,
    timepoints = NULL,
    duration = max(dplyr::count(modelled)$n),
    interval = 7,
    mapping = if (interfacer::is_col_present(modelled, class)) ggplot2::aes(colour = class) else ggplot2::aes(),
    cis = TRUE,
    ...
) {

  # N.B. standard pdf device does not support unicode for
  # doubling time interval and grid generates a warning that
  # cannot be suppressed.
  # conversion failure on '±<inf>' in 'mbcsToSbcs': for <inf> (U+221E)"

  if (is.null(timepoints)) timepoints = max(modelled$time)

  timepoints = rlang::exec(as.time_period, timepoints, !!!.get_meta(modelled$time))

  interfacer::idispatch(
    modelled,
    plot_growth_phase.incidence_per_capita = i_incidence_per_capita_rate,
    plot_growth_phase.incidence = i_incidence_rate,
    plot_growth_phase.risk_ratio = i_risk_ratio_rate,
    plot_growth_phase.proportion = i_proportion_rate,
  )
}

# internal function for dispatch
plot_growth_phase.incidence = function(
    modelled = i_incidence_rate,
    timepoints = NULL,
    duration = max(dplyr::count(modelled)$n),
    interval = 7,
    mapping = if (interfacer::is_col_present(modelled, class)) ggplot2::aes(colour = class) else ggplot2::aes(),
    cis = TRUE,
    ...
) {

  # modelled = interfacer::ivalidate(modelled)

  modelled = modelled %>% dplyr::mutate(
    x=growth.0.5, ymin=incidence.0.025, ymax=incidence.0.975,
    y=incidence.0.5, xmin=growth.0.025, xmax=growth.0.975
  )

  .do_phase(modelled, timepoints, duration, interval, mapping, ...,
            xlab = "growth rate per %s",
            ylab = "cases per %s",
            cis = cis,
            show_doubling = TRUE
  )+scale_y_log1p()

}

# internal function for dispatch
plot_growth_phase.incidence_per_capita = function(
    modelled = i_incidence_per_capita_rate,
    timepoints = NULL,
    duration = max(dplyr::count(modelled)$n),
    interval = 7,
    mapping = if (interfacer::is_col_present(modelled, class)) ggplot2::aes(colour = class) else ggplot2::aes(),
    cis = TRUE,
    ...
) {

  # modelled = interfacer::ivalidate(modelled)

  modelled = modelled %>% dplyr::mutate(
    x=growth.0.5, ymin=incidence.per_capita.0.025, ymax=incidence.per_capita.0.975,
    y=incidence.per_capita.0.5, xmin=growth.0.025, xmax=growth.0.975
  )

  population_unit = unique(modelled$population_unit)

  .do_phase(modelled, timepoints, duration, interval, mapping, ...,
            xlab = "growth rate per %s",
            ylab = sprintf("cases per %s per %%s", .fmt_pop(population_unit)),
            cis = cis,
            show_doubling = TRUE
  )+
  scale_y_log1p()

}

# internal function for dispatch
plot_growth_phase.proportion = function(
    modelled = i_proportion_rate,
    timepoints = NULL,
    duration = max(dplyr::count(modelled)$n),
    interval = 7,
    mapping = if (interfacer::is_col_present(modelled, class)) ggplot2::aes(colour = class) else ggplot2::aes(),
    cis = TRUE,
    ...
) {

  modelled = interfacer::ivalidate(modelled)

  modelled = modelled %>% dplyr::mutate(
    x=relative.growth.0.5, ymin=proportion.0.025*100, ymax=proportion.0.975*100,
    y=proportion.0.5*100, xmin=relative.growth.0.025, xmax=relative.growth.0.975
  )

  .do_phase(modelled, timepoints, duration, interval, mapping, ...,
            xlab = "relative growth rate per %s",
            ylab = "proportion (%%)",
            cis = cis
  )
}


plot_growth_phase.risk_ratio = function(
    modelled = i_risk_ratio_rate,
    timepoints = NULL,
    duration = max(dplyr::count(modelled)$n),
    interval = 7,
    mapping = if (interfacer::is_col_present(modelled, class)) ggplot2::aes(colour = class) else ggplot2::aes(),
    cis = TRUE,
    ...
) {

  modelled = interfacer::ivalidate(modelled)

  modelled = modelled %>% dplyr::mutate(
    x=relative.growth.0.5, ymin=risk_ratio.0.025, ymax=risk_ratio.0.975,
    y=risk_ratio.0.5, xmin=relative.growth.0.025, xmax=relative.growth.0.975
  )

  my = ceiling(max(modelled$risk_ratio.0.975,na.rm=TRUE))
  # TODO: sometimes this will include data that is not on the plot
  # mx = .glimit(modelled$relative.growth.0.5)

  p = .do_phase(modelled, timepoints, duration, interval, mapping, ...,
            xlab = "relative growth rate per %s",
            ylab = "relative risk",
            cis = cis
  )

  p$layers = c(
    ggplot2::geom_hline(yintercept = 1, colour="grey50"), p$layers)

  p+
  ggplot2::scale_y_log10()+
  suppressWarnings(ggplot2::coord_cartesian(ylim = c(1/my,my)))

}


## Common function ----

# internal function for dispatch
.do_phase = function(
    modelled,
    timepoints = NULL,
    duration = max(dplyr::count(modelled)$n),
    interval = 7,
    mapping = ggplot2::aes(colour = class),
    ...,
    xlab,
    ylab,
    cis,
    point_size = ggplot2::GeomPoint$default_aes$size,
    show_doubling = FALSE
) {

  if (isFALSE(interval)) interval = duration+1
  grps = modelled %>% dplyr::groups()



  plot_data = modelled %>%
    dplyr::cross_join(tibble::tibble(
      end = timepoints,
      labels = labels(timepoints, ifmt="{start}", dfmt="%d %b %Y")
    )) %>%
    dplyr::group_by(!!!grps,end,labels) %>%
    dplyr::filter(time <= end) %>%
    dplyr::arrange(dplyr::desc(time)) %>%
    dplyr::filter(time > end-duration) %>%
    dplyr::mutate(
      fade = 1-0.9*(dplyr::row_number()-1)/duration,
      display_point = (dplyr::row_number()-1) %% interval == 0,
      display_errorbars = (dplyr::row_number()-1) == 0,
      point_size = point_size * (0.6-0.6*(dplyr::row_number()-1)/duration+
                                   ifelse((dplyr::row_number()-1) == 0, 0.4, 0))
    )

  mx = .glimit(plot_data$x)

  out = ggplot2::ggplot(data=plot_data)+
    ggplot2::geom_vline(xintercept = 0, colour="grey50")+
    .layer(ggplot2::GeomPoint,
           data=plot_data %>% dplyr::filter(display_point),
           mapping=ggplot2::aes(x=x, y=y, alpha=fade, size=point_size, !!!mapping),
           ...
    )+
    .layer(ggplot2::GeomPath,
           mapping=ggplot2::aes(x=x, y=y, alpha=fade*0.75, !!!mapping),
           ...
    )+
    {if (cis) {ggplot2::geom_errorbar(data=plot_data %>% dplyr::filter(display_errorbars), mapping=ggplot2::aes(x=x, ymin=ymin, ymax=ymax, !!!mapping),width=0)} else {NULL}}+
    {if (cis) {ggplot2::geom_errorbarh(data=plot_data %>% dplyr::filter(display_errorbars), mapping=ggplot2::aes(y=y, xmin=xmin, xmax=xmax, !!!mapping),height=0)} else {NULL}}+
    ggplot2::scale_alpha_identity()+
    ggplot2::scale_size_identity()+
    ggplot2::facet_wrap(~ labels, strip.position = "right")+
    ggplot2::xlab(suppressWarnings(sprintf(xlab, .fmt_unit(modelled$time))))+
    ggplot2::ylab(suppressWarnings(sprintf(ylab, .fmt_unit(modelled$time))))+
    ggplot2::theme(legend.title=ggplot2::element_blank()) +
    {
      if (show_doubling) {
          # Doubling time is not easy to interpret for realtive growth rates
          ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(
            labels = function(x) ifelse(x==0,.pdf_safe("\u00B1\u221E"),sprintf("%.2g",(log(2)/x/.step(modelled$time)))),
            name="doubling time (days)"))
      } else {
        NULL
      }
    }+
    ggplot2::coord_cartesian(xlim = c(-mx,mx))

  return(out)

}



