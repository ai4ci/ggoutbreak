% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import-standalone-mixture-distributions.R
\name{.pmixnorm}
\alias{.pmixnorm}
\title{The cumulative density function of a mixture of normal distributions}
\usage{
.pmixnorm(q, means, sds, weights = 1, na.rm = FALSE)
}
\arguments{
\item{q}{vector of quantiles.}

\item{means}{a vector of normal distribution means}

\item{sds}{a vector of normal distribution sds}

\item{weights}{a vector of weights}

\item{na.rm}{remove distributions which have NA for mean or sd}
}
\value{
the pdf of the mixture distribution.
}
\description{
The cumulative density function of a mixture of normal distributions
}
\examples{
try({
.pmixnorm(q=c(2,20), means=c(10,13,14), sds=c(1,1,2), weights=c(2,2,3))
})
}
\concept{distributions}
\keyword{internal}
