% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimator-rt-renewal.R
\name{rt_from_renewal}
\alias{rt_from_renewal}
\title{Reproduction number from renewal equation applied to modelled incidence
using statistical re-sampling}
\usage{
rt_from_renewal(
  df = i_incidence_model,
  ip = i_discrete_ip,
  bootstraps = 1000,
  seed = Sys.time(),
  .progress = interactive()
)
}
\arguments{
\item{df}{modelled incidence estimate - a dataframe with columns:
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

\item{ip}{infectivity profile - a dataframe with columns:
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

\item{.progress}{show a CLI progress bar}
}
\value{
A dataframe containing the following columns:
\itemize{
\item time (ggoutbreak::time_period + group_unique) - A (usually complete) set of singular observations per unit time as a \code{time_period}
\item rt.fit (double) - an estimate of the reproduction number
\item rt.se.fit (positive_double) - the standard error of the reproduction number
\item rt.0.025 (double) - lower confidence limit of the reproduction number
\item rt.0.5 (double) - median estimate of the reproduction number
\item rt.0.975 (double) - upper confidence limit of the reproduction number
}

Any grouping allowed.
}
\description{
Calculate a reproduction number estimate from modelled incidence estimates,
by statistical sampling from a log-normally distributed incidence estimate,
such as you get from a \verb{I_t ~ Poisson(I_0 e^\{rt\})}  model using a log link
function. This is combined with uncertain infectivity profile specified as
multiple discrete empirical distributions, to calculate the range of possible
values for $R_t$.
}
\details{
This method is moderately slow and non deterministic by default.
}
\examples{
data = test_poisson_rt_smooth

tmp2 = data \%>\%
   ggoutbreak::poisson_locfit_model() \%>\%
   ggoutbreak::rt_from_renewal(test_ip)

if (interactive()) {
  plot_rt(tmp2, date_labels="\%b \%y")+sim_geom_function(data,colour="red")
}

}
\concept{models}
