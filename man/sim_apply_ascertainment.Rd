% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation-utils.R
\name{sim_apply_ascertainment}
\alias{sim_apply_ascertainment}
\title{Apply a ascertainment bias to the observed case counts.}
\usage{
sim_apply_ascertainment(df = i_sim_count_data, fn_asc = ~1, seed = Sys.time())
}
\arguments{
\item{df}{a count dataframe from e.g. \code{\link[=sim_poisson_model]{sim_poisson_model()}} or
\code{\link[=sim_summarise_linelist]{sim_summarise_linelist()}} - a dataframe with columns:
\itemize{
\item statistic (character) - An identifier for the statistic, whether that be infections, admissions, deaths
\item count (positive_integer) - Positive case counts associated with the specified time frame
\item time (ggoutbreak::time_period + group_unique) - A (usually complete) set of singular observations per unit time as a `time_period`
}

Minimally grouped by: statistic (and other groupings allowed).
}

\item{fn_asc}{a function that takes a single input vector \code{t} and returns a
probability of ascertainment, e.g. \code{~ stats::rbeta(.x, 20, 80)} or
\verb{~ rbeta2(.x,prob=<probability>,kappa=<dispersion>)}. or
\code{\link[=cfg_weekly_proportion_rng]{cfg_weekly_proportion_rng()}}}

\item{seed}{a RNG seed}
}
\value{
a dataframe with \code{original} column, and \code{count} column modified to
include ascertainment bias.
}
\description{
Apply a ascertainment bias to the observed case counts.
}
\examples{
tibble::tibble(
  statistic = "incidence",
  time=as.time_period(1:10,"1 day"),
  count=rep(100,10)
) \%>\% dplyr::group_by(statistic) \%>\% sim_apply_ascertainment(~ ifelse(.x<=5,0.1,0.9))
}
\concept{test}
