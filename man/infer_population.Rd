% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infer-population.R
\name{infer_population}
\alias{infer_population}
\title{Infers a daily baseline population for a timeseries}
\usage{
infer_population(df = i_timeseries, pop = i_population_data)
}
\arguments{
\item{df}{A time series, or a grouped collection of time series. - a dataframe with columns:
\itemize{
\item time (ggoutbreak::time_period + group_unique) - A (usually complete) set of singular observations per unit time as a `time_period`
}

Any grouping allowed.
}

\item{pop}{The population data must be grouped in the same way as \code{df}. It
might also have a \code{time} column as a \code{time_period} if the population is not
static - a dataframe with columns:
\itemize{
\item population (positive_integer) - Size of population
}

Any grouping allowed.
}
}
\value{
the \code{df} timeseries with additional \code{population} column
}
\description{
This function augments any timeseries with a population denominator. The
population data may be static estimates, or a set of estimates at time points.
The population data may be grouped in which case the grouping might be geographical
area or age group or gender for example.
The two inputs must have compatible grouping (i.e. all the groups in
the population data must be present in the timeseries).
}
\examples{
ggoutbreak::england_covid \%>\%
  ggoutbreak::infer_population(ggoutbreak::england_demographics) \%>\%
  dplyr::glimpse()
}
\concept{models}
