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
#' @param x_axis_style should the x-axis be `"date"` or a count of
#'   `"time_period"`s since a start date (may be specified in `...` or defaults
#'   from data)
#' @inheritDotParams ggplot2::scale_x_date -sec.axis
#' @inheritDotParams as.time_period unit start_date anchor
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
  x_axis_style = c("date", "time_period"),
  ...
) {
  if (is.null(events)) {
    events = interfacer::iproto(i_events)
  }
  events = interfacer::ivalidate(events)
  rects = events %>% dplyr::filter(!is.na(end))
  lines = events %>% dplyr::filter(is.na(end))
  x_axis_style = match.arg(x_axis_style)

  event_label_angle = ((event_label_angle + 180) %% 360) - 180

  dots = rlang::enexprs(...)
  scale_dots = dots[names(dots) %in% names(formals(ggplot2::scale_x_date))]
  scale_dots$sec.axis = ggplot2::dup_axis(
    name = "",
    breaks = .x_axis(events$start, x_axis_style = x_axis_style, ...),
    labels = events$label,
    guide = guide_axis
  )

  tmp = list(
    ggplot2::geom_rect(
      data = rects,
      mapping = ggplot2::aes(
        xmin = .x_axis(start, x_axis_style = x_axis_style, ...),
        xmax = .x_axis(end, x_axis_style = x_axis_style, ...)
      ),
      inherit.aes = FALSE,
      ymin = -Inf,
      ymax = Inf,
      fill = event_fill_colour,
      colour = NA,
      alpha = 0.25
    ),
    ggplot2::geom_vline(
      data = lines,
      mapping = ggplot2::aes(
        xintercept = .x_axis(start, x_axis_style = x_axis_style, ...)
      ),
      colour = event_line_colour,
      show.legend = FALSE
    )
  )

  if (!hide_labels) {
    tmp = c(
      tmp,
      list(
        if (x_axis_style == "date") {
          list(
            do.call(ggplot2::scale_x_date, scale_dots),
            ggplot2::xlab(NULL)
          )
        } else {
          ggplot2::xlab(sprintf(
            "time (%ss)",
            .fmt_unit(
              .get_meta(.x_axis(0, x_axis_style = x_axis_style, ...))$unit
            )
          ))
        },
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

#' Add a "true" time series to a plot
#'
#' @param true_df a data frame with a `time_period` column called time and a
#'   value column (name given in `true_col`). This is optional and will be picked
#'   up from the `raw` parameter if given.
#' @inheritDotParams as.time_period unit start_date anchor
#' @param true_col the column name / expression of the true value
#' @param true_fmt a list of ggplot formatting to apply to the true value timeseries
#'
#' @returns A geom with the true value as a line.
#' @keywords internal
#'
#' @concept vis
geom_truth = function(
  true_df = NULL,
  ...,
  true_col = NULL,
  true_fmt = list(colour = "red")
) {
  true_col = rlang::enexpr(true_col)
  if (is.null(true_col)) {
    return(NULL)
  }
  if (is.null(true_df)) {
    true_df = rlang::list2(...)$raw
    if (is.null(true_df)) {
      .message_once(
        "Could not find data for true timeseries from `true_df` or `raw` parameters"
      )
      return(NULL)
    }
  }
  return(
    .layer(
      ggplot2::GeomLine,
      data = true_df,
      mapping = ggplot2::aes(
        x = .x_axis(time, ...),
        y = !!true_col
      ),
      !!!true_fmt
    )
  )
}

.x_axis = function(x, x_axis_style = "date", ...) {
  if (x_axis_style == "date") {
    return(as.Date(x))
  } else {
    # Numeric / time period
    return(as.time_period(x, ...))
  }
}

# TODO: look at how this is done with the units package:
# https://github.com/r-quantities/units/blob/main/R/scale_units.R
