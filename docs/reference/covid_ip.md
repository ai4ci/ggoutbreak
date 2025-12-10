# A COVID-19 infectivity profile based on an empirical resampling approach

An infectivity profile derived from a meta-analysis of serial intervals.

## Usage

``` r
data(covid_ip)
```

## Format

A dataframe containing the following columns:

- boot (anything + default(1)) - a bootstrap identifier

- probability (proportion) - the probability of infection between
  previous time period until `time`

- time (double) - the end of the time period (in days)

Must be grouped by: boot (exactly).

A dataframe containing the following columns:

- boot (anything + default(1)) - a bootstrap identifier

- probability (proportion) - the probability of infection between
  previous time period until `time`

- tau (numeric) - the time index this probability relates to (in days)

- a0 (numeric) - the beginning of the time period

- a1 (numeric) - the end of the time period

Grouped by: boot.

1400 rows and 5 columns
