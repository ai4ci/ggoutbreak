% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infer-prevalence.R
\name{infer_prevalence}
\alias{infer_prevalence}
\title{Infer the prevalence of disease from incidence estimates and population size.}
\usage{
infer_prevalence(
  modelled = i_incidence_model,
  pop = i_population_data,
  ip = i_discrete_ip,
  bootstraps = 1000,
  seed = Sys.time(),
  ...
)
}
\arguments{
\item{modelled}{Model output from processing the \code{raw} dataframe with something like
\code{poission_locfit_model} - a dataframe with columns:
\itemize{
\item time (ggoutbreak::time_period + group_unique) - A (usually complete) set of singular observations per unit time as a `time_period`
\item incidence.fit (double) - an estimate of the incidence rate on a log scale
\item incidence.se.fit (positive_double) - the standard error of the incidence rate estimate on a log scale
\item incidence.0.025 (positive_double) - lower confidence limit of the incidence rate (true scale)
\item incidence.0.5 (positive_double) - median estimate of the incidence rate (true scale)
\item incidence.0.975 (positive_double) - upper confidence limit of the incidence rate (true scale)
}

Any grouping allowed.
}

\item{pop}{The population data must be grouped in the same way as \code{modelled}. - a dataframe with columns:
\itemize{
\item population (positive_integer) - Size of population
}

Any grouping allowed.
}

\item{ip}{A discrete distribution representing the probability
of detection of disease on a given day after infection.
This will not necessarily sum to one, but each entry will not exceed the
asymptomatic fraction. - a dataframe with columns:
\itemize{
\item boot (anything + default(1)) - a bootstrap identifier
\item probability (proportion) - the probability of new event during this period.
\item tau (integer + complete) - the days since the index event.
}

Minimally grouped by: boot (and other groupings allowed).

A default value is defined.
}

\item{bootstraps}{the number of samples to take at each time point. This will
be rounded up to a whole multiple of the infectivity profile distribution
length.}

\item{seed}{a random number seed for reproducibility}

\item{...}{not used}
}
\value{
the modelled input with additional proportion columns
}
\description{
Log-scaled incidence estimates are used to generate samples of incidence.
These are convolved with the infectivity profile (as a set of discrete distributions)
and the result is evaluated as a fraction of population. The result is
fitted to a logit-normal (using moments) and quantiles produced from samples.
}
\examples{
tmp = ggoutbreak::test_poisson_rt_smooth \%>\%
  ggoutbreak::poisson_locfit_model(window=14) \%>\%
  ggoutbreak::infer_prevalence(
     pop = 10000,
     ip = ggoutbreak::test_ip
  )

if(interactive()) {
  plot_prevalence(tmp)
}
}
\concept{models}
