% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{du_serial_interval_ip}
\alias{du_serial_interval_ip}
\title{The Du empirical serial interval dataset}
\format{
A dataframe containing the following columns:
\itemize{
\item boot (anything + default(1)) - a bootstrap identifier
\item probability (proportion) - the probability of infection between previous time period until \code{time}
\item tau (numeric) - the time index this probability relates to (in days)
\item a0 (numeric) - the beginning of the time period
\item a1 (numeric) - the end of the time period
}

Grouped by: boot.

2603 rows and 5 columns
}
\usage{
data(du_serial_interval_ip)
}
\description{
From Z. Du, X. Xu, Y. Wu, L. Wang, B. J. Cowling, and L. A. Meyers, ‘Serial Interval of COVID-19 among Publicly Reported Confirmed Cases’, Emerg Infect Dis, vol. 26, no. 6, pp. 1341–1343, Jun. 2020, doi: 10.3201/eid2606.200357.
}
\concept{datasets}
\keyword{datasets}
