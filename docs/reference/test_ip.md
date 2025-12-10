# A test infectivity profile generated from a set of discretised gamma distributions with parameters mean 5 (95% CI 4-6) and sd 2 (95% CI 1.5-2.5).

A test infectivity profile generated from a set of discretised gamma
distributions with parameters mean 5 (95% CI 4-6) and sd 2 (95% CI
1.5-2.5).

## Usage

``` r
data(example_ip())
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

2000 rows and 5 columns
