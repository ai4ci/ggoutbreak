% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-proportion-ts.R
\name{plot_proportion}
\alias{plot_proportion}
\title{Plot a proportions timeseries}
\usage{
plot_proportion(
  modelled = i_proportion_model,
  raw = i_proportion_data,
  ...,
  mapping = .check_for_aes(modelled, ...),
  events = i_events
)
}
\arguments{
\item{modelled}{Proportion model estimates - a dataframe with columns:
\itemize{
\item time (ggoutbreak::time_period + group_unique) - A (usually complete) set of singular observations per unit time as a `time_period`
\item proportion.fit (double) - an estimate of the proportion on a logit scale
\item proportion.se.fit (positive_double) - the standard error of proportion estimate on a logit scale
\item proportion.0.025 (proportion) - lower confidence limit of proportion (true scale)
\item proportion.0.5 (proportion) - median estimate of proportion (true scale)
\item proportion.0.975 (proportion) - upper confidence limit of proportion (true scale)
}

Any grouping allowed.
}

\item{raw}{Raw count data with denominator - a dataframe with columns:
\itemize{
\item denom (positive_integer) - Total test counts associated with the specified time frame
\item count (positive_integer) - Positive case counts associated with the specified time frame
\item time (ggoutbreak::time_period + group_unique) - A (usually complete) set of singular observations per unit time as a `time_period`
}

Any grouping allowed.
}

\item{...}{
  Named arguments passed on to \code{\link[=geom_events]{geom_events}}\describe{
    \item{\code{event_label_size}}{how big to make the event label}
\item{\code{event_label_colour}}{the event label colour}
\item{\code{event_label_angle}}{the event label colour}
\item{\code{event_line_colour}}{the event line colour}
\item{\code{event_fill_colour}}{the event area fill}
\item{\code{hide_labels}}{do not show labels at all}
\item{\code{guide_axis}}{a guide axis configuration for the labels
(see \code{ggplot2::guide_axis} and \code{ggplot2::dup_axis}). This can be used to
specify a position amongst other things.}
\item{\code{...}}{
  Named arguments passed on to \code{\link[ggplot2:scale_date]{ggplot2::scale_x_date}}\describe{
    \item{\code{name}}{The name of the scale. Used as the axis or legend title. If
\code{waiver()}, the default, the name of the scale is taken from the first
mapping used for that aesthetic. If \code{NULL}, the legend title will be
omitted.}
\item{\code{breaks}}{One of:
\itemize{
\item \code{NULL} for no breaks
\item \code{waiver()} for the breaks specified by \code{date_breaks}
\item A \code{Date}/\code{POSIXct} vector giving positions of breaks
\item A function that takes the limits as input and returns breaks as output
}}
\item{\code{date_breaks}}{A string giving the distance between breaks like "2
weeks", or "10 years". If both \code{breaks} and \code{date_breaks} are
specified, \code{date_breaks} wins. Valid specifications are 'sec', 'min',
'hour', 'day', 'week', 'month' or 'year', optionally followed by 's'.}
\item{\code{labels}}{One of:
\itemize{
\item \code{NULL} for no labels
\item \code{waiver()} for the default labels computed by the
transformation object
\item A character vector giving labels (must be same length as \code{breaks})
\item An expression vector (must be the same length as breaks). See ?plotmath for details.
\item A function that takes the breaks as input and returns labels
as output. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
}}
\item{\code{date_labels}}{A string giving the formatting specification for the
labels. Codes are defined in \code{\link[=strftime]{strftime()}}. If both \code{labels}
and \code{date_labels} are specified, \code{date_labels} wins.}
\item{\code{minor_breaks}}{One of:
\itemize{
\item \code{NULL} for no breaks
\item \code{waiver()} for the breaks specified by \code{date_minor_breaks}
\item A \code{Date}/\code{POSIXct} vector giving positions of minor breaks
\item A function that takes the limits as input and returns minor breaks as
output
}}
\item{\code{date_minor_breaks}}{A string giving the distance between minor breaks
like "2 weeks", or "10 years". If both \code{minor_breaks} and
\code{date_minor_breaks} are specified, \code{date_minor_breaks} wins. Valid
specifications are 'sec', 'min', 'hour', 'day', 'week', 'month' or 'year',
optionally followed by 's'.}
\item{\code{limits}}{One of:
\itemize{
\item \code{NULL} to use the default scale range
\item A numeric vector of length two providing limits of the scale.
Use \code{NA} to refer to the existing minimum or maximum
\item A function that accepts the existing (automatic) limits and returns
new limits. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
Note that setting limits on positional scales will \strong{remove} data outside of the limits.
If the purpose is to zoom, use the limit argument in the coordinate system
(see \code{\link[ggplot2:coord_cartesian]{coord_cartesian()}}).
}}
\item{\code{expand}}{For position scales, a vector of range expansion constants used to add some
padding around the data to ensure that they are placed some distance
away from the axes. Use the convenience function \code{\link[ggplot2:expansion]{expansion()}}
to generate the values for the \code{expand} argument. The defaults are to
expand the scale by 5\% on each side for continuous variables, and by
0.6 units on each side for discrete variables.}
\item{\code{oob}}{One of:
\itemize{
\item Function that handles limits outside of the scale limits
(out of bounds). Also accepts rlang \link[rlang:as_function]{lambda}
function notation.
\item The default (\code{\link[scales:oob]{scales::censor()}}) replaces out of
bounds values with \code{NA}.
\item \code{\link[scales:oob]{scales::squish()}} for squishing out of bounds values into range.
\item \code{\link[scales:oob]{scales::squish_infinite()}} for squishing infinite values into range.
}}
\item{\code{guide}}{A function used to create a guide or its name. See
\code{\link[ggplot2:guides]{guides()}} for more information.}
\item{\code{position}}{For position scales, The position of the axis.
\code{left} or \code{right} for y axes, \code{top} or \code{bottom} for x axes.}
\item{\code{sec.axis}}{\code{\link[ggplot2:sec_axis]{sec_axis()}} is used to specify a secondary axis.}
\item{\code{timezone}}{The timezone to use for display on the axes. The default
(\code{NULL}) uses the timezone encoded in the data.}
\item{\code{na.value}}{Missing values will be replaced with this value.}
}}
}
  Named arguments passed on to \code{\link[=proportion_locfit_model]{proportion_locfit_model}}\describe{
    \item{\code{d}}{the input - a dataframe with columns:
\itemize{
\item denom (positive_integer) - Total test counts associated with the specified time frame
\item count (positive_integer) - Positive case counts associated with the specified time frame
\item time (ggoutbreak::time_period + group_unique) - A (usually complete) set of singular observations per unit time as a `time_period`
}

Any grouping allowed.
}
\item{\code{...}}{not used and present to allow proportion model to be used in a
\code{group_modify}}
\item{\code{window}}{a number of data points defining the bandwidth of the estimate,
smaller values result in less smoothing, large value in more. The default
value of 14 is calibrated for data provided on a daily frequency, with
weekly data a lower value may be preferred. - default \code{14}}
\item{\code{deg}}{polynomial degree (min 1) - higher degree results in less
smoothing, lower values result in more smoothing. A degree of 1 is fitting
a linear model piece wise. - default \code{1}}
\item{\code{frequency}}{the density of the output estimates as a time period such as
\verb{7 days} or \verb{2 weeks}. - default \code{1 day}}
\item{\code{predict}}{result is a prediction dataframe. If false we return the
\code{locfit} models (advanced). - default \code{TRUE}}
\item{\code{.progress}}{show a CLI progress bar}
}}

\item{mapping}{a \code{ggplot2::aes} mapping. Most importantly setting the \code{colour}
to something if there are multiple incidence timeseries in the plot}

\item{events}{Significant events or time spans - a dataframe with columns:
\itemize{
\item label (character) - the event label
\item start (date) - the start date, or the date of the event
\item end (date) - the end date or NA if a single event
}

Any grouping allowed.

A default value is defined.
}
}
\value{
a ggplot object
}
\description{
Plot a proportions timeseries
}
\examples{
tmp = ggoutbreak::test_poisson_rt_2class \%>\%
  ggoutbreak::proportion_locfit_model(window=21) \%>\%
  dplyr::glimpse()

if(interactive()) {
  plot_proportion(tmp)+
    ggplot2::scale_fill_viridis_d(aesthetics = c("fill","colour"))
}
}
\concept{vis}
