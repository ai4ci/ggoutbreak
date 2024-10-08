% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{england_variants}
\alias{england_variants}
\title{Counts of COVID-19 variants}
\format{
A dataframe containing the following columns:
\itemize{
\item date (date) - the end date of the week
\item time (time_period) - the time column
\item class (enum(\code{Other},\code{Alpha (B.1.1.7)},\code{Delta (B.1.617.2)},\code{Delta (AY.4)},\code{Omicron (Other)},\code{Omicron (BA.2)},\code{Omicron (BA.4)},\code{Omicron (BA.5)},\code{XBB (Other)},\code{Kraken (XBB.1.5)},\code{Arcturus (XBB.1.16)},\code{Eris (EG.5.1)})) - the class column
\item who_class (enum(\code{Other},\code{Alpha},\code{Delta},\code{Omicron},\code{Kraken},\code{Arcturus},\code{Eris})) - the who_class column
\item count (numeric) - the weekly count column
\item denom (numeric) - the number of sequences performed in that week
}

Must be grouped by: class (and other groupings allowed).

No default value.

479 rows and 6 columns
}
\usage{
data(england_variants)
}
\description{
Data from the COG-UK and Sanger centre sequencing
programme. The data were made available through the Welcome foundation at
Lower tier local authority level, and is weekly timeseries of counts per
variant. Variants were assigned using the tree structure of the Pango
lineage. Different sub-lineages are aggregated to the major WHO variants of
concern.
}
\concept{datasets}
\keyword{datasets}
