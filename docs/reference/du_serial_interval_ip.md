# The Du empirical serial interval dataset

From Z. Du, X. Xu, Y. Wu, L. Wang, B. J. Cowling, and L. A. Meyers,
‘Serial Interval of COVID-19 among Publicly Reported Confirmed Cases’,
Emerg Infect Dis, vol. 26, no. 6, pp. 1341–1343, Jun. 2020, doi:
10.3201/eid2606.200357.

## Usage

``` r
data(du_serial_interval_ip)
```

## Format

A dataframe containing the following columns:

- boot (anything + default(1)) - a bootstrap identifier

- probability (proportion) - the probability of infection between
  previous time period until `time`

- tau (numeric) - the time index this probability relates to (in days)

- a0 (numeric) - the beginning of the time period

- a1 (numeric) - the end of the time period

Grouped by: boot.

2603 rows and 5 columns
