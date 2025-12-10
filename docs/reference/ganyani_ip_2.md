# A COVID-19 infectivity profile based on an Ganyani et al 2020

This version is discretised in a manner that makes it incompatible with
`EpiEstim`.

## Usage

``` r
data(ganyani_ip_2)
```

## Format

A dataframe containing the following columns:

- boot (anything + default(1)) - a bootstrap identifier

- probability (proportion) - the probability of infection between
  previous time period until `time`

- tau (double) - the time index this probability relates to (in days)

- a0 - the beginning of the time period

- a1 - the end of the time period

Grouped by boot (exactly).

A dataframe containing the following columns:

- boot (anything + default(1)) - a bootstrap identifier

- probability (proportion) - the probability of infection between
  previous time period until `time`

- tau (numeric) - the time index this probability relates to (in days)

- a0 (numeric) - the beginning of the time period

- a1 (numeric) - the end of the time period

Grouped by: boot.

2800 rows and 5 columns

## References

<https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.17.2000257>
