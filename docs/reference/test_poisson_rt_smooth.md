# Output of a poisson model simulation with a smooth function for \$R_t\$ defined as `R(t) = e^(sin(t/80*pi)^4-0.25))`. This is a relatively unchallenging test data set that should not pose a problem for smooth estimators.

This is generated using the central value of the `example_ip()`
infectivity profile

## Usage

``` r
data(test_poisson_rt_smooth)
```

## Format

A dataframe containing the following columns:

- time (as.time_period) - the time column

- rt (numeric) - the time varying rt column (parameters)

- imports (numeric) - the imports column

- rate (numeric) - the poisson rate column (underlying infection rate)

- count (integer) - the count column

- statistic (character) - the statistic column

Minimally grouped by: statistic (and other groupings allowed).

161 rows and 6 columns
