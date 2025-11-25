#' Add time series event markers to a time series plot.
#'
#' The x axis must be a date.
#'
#' @iparam events Significant events or time spans
#' @param event_label_size how big to make the event label
#' @param event_label_colour the event label colour
#' @param event_label_angle the event label colour
#' @param event_line_colour the event line colour
#' @param event_fill_colour the event area fill
#' @param hide_labels do not show labels at all
#' @param guide_axis a guide axis configuration for the labels
#'   (see `ggplot2::guide_axis` and `ggplot2::dup_axis`). This can be used to
#'   specify a position amongst other things.
#' @inheritDotParams ggplot2::scale_x_date -sec.axis
#'
#' @return a set of `geoms` for a time series.
#' @export
#'
#' @concept vis
geom_events = function(
  events = i_events,
  event_label_size = 7,
  event_label_colour = "black",
  event_label_angle = -30,
  event_line_colour = "grey50",
  event_fill_colour = "grey50",
  hide_labels = FALSE,
  guide_axis = ggplot2::derive(),
  ...
) {
  events = interfacer::ivalidate(events)
  rects = events %>% dplyr::filter(!is.na(end))
  lines = events %>% dplyr::filter(is.na(end))

  event_label_angle = ((event_label_angle + 180) %% 360) - 180

  dots = rlang::list2(...)
  scale_dots = dots[names(dots) %in% names(formals(ggplot2::scale_x_date))]
  scale_dots$sec.axis = ggplot2::dup_axis(
    name = "",
    breaks = events$start,
    labels = events$label,
    guide = guide_axis
  )

  tmp = list(
    ggplot2::geom_rect(
      data = rects,
      mapping = ggplot2::aes(xmin = start, xmax = end),
      inherit.aes = FALSE,
      ymin = -Inf,
      ymax = Inf,
      fill = event_fill_colour,
      colour = NA,
      alpha = 0.25
    ),
    ggplot2::geom_vline(
      data = lines,
      mapping = ggplot2::aes(xintercept = start),
      colour = event_line_colour,
      show.legend = FALSE
    )
  )

  if (!hide_labels) {
    tmp = c(
      tmp,
      list(
        do.call(ggplot2::scale_x_date, scale_dots),
        ggplot2::theme(
          axis.text.x.top = ggplot2::element_text(
            angle = event_label_angle,
            vjust = as.numeric(event_label_angle > 0),
            hjust = as.numeric(event_label_angle < 0),
            colour = event_label_colour,
            size = event_label_size
          )
        )
      )
    )
  }

  return(tmp)
}

# TODO: look at how this is done with the units package:
# https://github.com/r-quantities/units/blob/main/R/scale_units.R
