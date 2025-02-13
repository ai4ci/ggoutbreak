% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation-utils.R
\name{cfg_gamma_ip_fn}
\alias{cfg_gamma_ip_fn}
\title{Get a IP generating function from time varying mean and SD of a gamma function}
\usage{
cfg_gamma_ip_fn(mean_fn = ~2, sd_fn = function(mean) sqrt(mean))
}
\arguments{
\item{mean_fn}{a function which gives the time-varying mean of a gamma
distribution, The function will be called minimally with \code{.x} or \code{t} which
will be the time as a time period. Other variables may be present.}

\item{sd_fn}{a function which gives the time-varying mean of a gamma
distribution. The function will be called with minimally \code{.x} or \code{t} which
will be the time period and \code{.y} or \code{mean} will be the mean. Other variables
may be present.}
}
\value{
a time dependent function that inputs a time (as a time_period) and
returns an ip delay distribution for each day defined by the \code{mean_fn} and \code{sd_fn}
}
\description{
Get a IP generating function from time varying mean and SD of a gamma function
}
\examples{
fn = cfg_gamma_ip_fn(mean_fn = \(t) ifelse(t < 5, 4, 2))
# a gamma function that changes mean at time 5
fn(4)
fn(7)
}
\concept{test}
