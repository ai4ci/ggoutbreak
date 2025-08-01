% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation-utils.R
\name{sim_branching_process}
\alias{sim_branching_process}
\title{Generate a line list from a branching process model parametrised by
reproduction number}
\usage{
sim_branching_process(
  changes = tibble::tibble(t = c(0, 40), rt = c(2.5, 0.8)),
  max_time = 80,
  seed = Sys.time(),
  fn_Rt = cfg_step_fn(changes),
  fn_ip = ~ggoutbreak::test_ip,
  fn_kappa = ~1,
  imports_df = NULL,
  fn_imports = ~ifelse(.x == 0, 30, 0),
  fn_list_next_gen = list(),
  ...
)
}
\arguments{
\item{changes}{a dataframe containing a \code{t} time column and \code{R} reproduction
number parameter. This parameter is optional if \code{fn_Rt} is specified}

\item{max_time}{maximum duration of simulation}

\item{seed}{random seed}

\item{fn_Rt}{can be specified instead of \code{changes} df. This is a vectorised
function that accepts a time parameter and returns a reproduction number.
If both this and \code{changes} are specified this takes preference.}

\item{fn_ip}{a function that takes input vector \code{t} (and/or \code{class}) and
returns an infectivity profile at times \code{t}.}

\item{fn_kappa}{a vectorised function taking \code{t} and other imported case
metadata returning a dispersion parameter controlling the likelihood of
individual super-spreading. This must be between 1 and \code{Inf} with 1 being
standard poisson dispersion and larger values representing over dispersion.}

\item{imports_df}{a data frame containing minimally \code{time} and \code{count} columns
plus any metadata about the imports in additional columns. Metadata columns
can inform the \code{fn_Rt},\code{fn_kappa} and \code{fn_ip} functions as additional parameters.}

\item{fn_imports}{a time varying function the defines the number of infected
importations. If \code{imports_df} is defined then this is used instead}

\item{fn_list_next_gen}{a named list of functions. The name corresponds to
metadata columns in the simulation, the function is a \code{purrr} style mapping that
will replace the old value in the named column with a new one. Such a function can be
generated with \code{\link[=cfg_transition_fn]{cfg_transition_fn()}} when a transition probability matrix is involved,
of it can be specified directly as a \code{case_when} style function. The function
must be vectorised and assume no grouping structure. If the function has
named parameters it can reference any of the metadata columns, or time
(as \code{t}). The \code{\link[=rcategorical]{rcategorical()}} function may be useful in this scenario.}

\item{...}{not used}
}
\value{
a line list of cases for a simulated outbreak

A dataframe containing the following columns: 
\itemize{
\item id (unique_id) - Patient level unique id
\item time (ggoutbreak::time_period) - Time of infection. A `time_period`
}

Any grouping possible.
}
\description{
Generate a line list from a branching process model parametrised by
reproduction number
}
\examples{
tmp = sim_branching_process(
  changes = tibble::tibble(t = c(0,40), R = c(1.5,0.8)),
  max_time = 120,
  seed = 100,
  fn_imports = ~ ifelse(.x<10,1,0)
)

if(interactive()) {
  plot_cases(tmp, mapping=ggplot2::aes(fill=as.factor(generation)),linewidth=0.1)
}

# imports can also be specified as a dataframe, which allows additional
# metadata in the line list. An example of which is as follows:
imports_df = tibble::tribble(
  ~time, ~variant, ~count,
  0:4, "wild-type", 100,
  10:14, "alpha", 5,
)
}
\concept{test}
