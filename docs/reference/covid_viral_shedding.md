# The COVID-19 viral shedding duration

The COVID-19 viral shedding duration

## Usage

``` r
data(covid_viral_shedding)
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

2600 rows and 3 columns

## References

<https://www.nature.com/articles/s41467-020-20568-4> From Von Kampen et
al.
