% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{england_covid_pcr_positivity}
\alias{england_covid_pcr_positivity}
\title{England COVID-19 PCR test positivity}
\format{
A dataframe containing the following columns:
\itemize{
\item date (date) - a daily time series
\item time (time_period) - the time column
\item count (numeric) - test positives in England on that day
\item denom (numeric) - total tests conducted on that day
}

No mandatory groupings.

No default value.

1413 rows and 4 columns
}
\usage{
data(england_covid_pcr_positivity)
}
\description{
The \code{coronavirus.gov.uk} dashboard published tests conducted and positive
results as separate data sets for a range of geographies. In this case the
data is combined with testing rate as denominator, and test positives as
count for the whole of England.
}
\concept{datasets}
\keyword{datasets}
