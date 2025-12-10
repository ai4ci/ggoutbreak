# A simulation dataset determined by a step function of growth rates. This is useful for demonstrating growth rate estimators.

A simulation dataset determined by a step function of growth rates. This
is useful for demonstrating growth rate estimators.

## Usage

``` r
data(test_poisson_growth_rate)
```

## Format

A dataframe containing the following columns:

- time (as.time_period) - the time column

- growth (numeric) - the time varying growth rate column (input
  parameter)

- imports (numeric) - the imports column

- rate (numeric) - the poisson rate column

- count (integer) - the sampled count column

- statistic (character) - the statistic column (infections)

Minimally grouped by: statistic (and other groupings allowed).

105 rows and 6 columns
