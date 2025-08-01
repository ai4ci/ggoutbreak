% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation-utils.R
\name{sim_apply_delay.linelist}
\alias{sim_apply_delay.linelist}
\title{Augment a line list of infection with a set of events}
\usage{
sim_apply_delay.linelist(
  df = i_sim_linelist,
  ...,
  fn_p_symptomatic = ~0.5,
  fn_symptom_delay = ~rgamma2(.x, mean = 5),
  fn_p_admitted = ~0.1,
  fn_admission_delay = cfg_weekly_gamma_rng(c(8, 8, 8, 8, 8, 9.5, 9)),
  fn_p_died = ~0.05,
  fn_death_delay = ~rgamma2(.x, mean = 14),
  fn_p_tested = ~0.8,
  fn_sample_delay = cfg_weekly_gamma_rng(c(1, 1, 1, 1, 1, 1.5, 1.4)),
  fn_result_delay = cfg_weekly_gamma_rng(c(1, 1, 1, 1, 1, 1.6, 1.5)),
  seed = Sys.time()
)
}
\arguments{
\item{df}{a line list dataframe arising from e.g. \code{\link[=sim_branching_process]{sim_branching_process()}} - a dataframe with columns:
\itemize{
\item id (unique_id) - Patient level unique id
\item time (ggoutbreak::time_period) - Time of infection. A `time_period`
}

Any grouping allowed.
}

\item{fn_p_symptomatic, fn_p_admitted, fn_p_died, fn_p_tested}{Function that
returns a probability between 0 and 1 for each row of the input dataframe.
A \code{purrr} style lambda is OK (e.g. \code{~ 1} for always true) the first
parameter of this will be time of infection. The function must be
vectorised on its inputs (and consume additional inputs with \code{...})}

\item{fn_symptom_delay, fn_admission_delay, fn_death_delay, }{a function that
calculates the time to event onset from infection. This will be called with
a vector of infection times as the first parameter (\code{time}) but all other
columns of \code{df} are also available as well as the \code{symptomatic},\code{died},and
\code{admitted} flags. The function must be vectorised on its inputs (and
consume additional inputs with \code{...}). A \code{purrr} style lambda is OK e.g. \code{~ stats::rgamma(.x, shape = 3)}, and the first parameter will be infection
time. if you have an discrete probability profile for this then you can use
\code{cfg_ip_sampler_rng(ip_symptoms)}.}

\item{fn_sample_delay}{This function returns the time from either symptom
onset (symptomatic) or from infection (asymptomatic) until a sample is
taken. (N.B. this might be better to do a screening test probability plus
screening test frequency rather than overloading this.)}

\item{fn_result_delay}{Identical to other functions except the first
parameter will be \code{sample_time} rather than time of infection. This is the
time from sampling to the result being available.}

\item{seed}{RNG seed for reproducibility}
}
\value{
a line list with additional time and delay columns.
}
\description{
Events include symptom onset, admission, death, test sampling, test
processing
}
\examples{
tmp = sim_branching_process(
  changes = tibble::tibble(t = c(0,20,40,60,80,110), R = c(1.8,1.5,0.9,1.5,0.8,1.2)),
  max_time = 120,
  seed = 100
)

tmp2 = tmp \%>\% sim_apply_delay()
tmp2 \%>\% dplyr::glimpse()

}
\keyword{internal}
