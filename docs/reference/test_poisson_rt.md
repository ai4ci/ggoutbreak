# An example of the linelist output of the poisson model simulation with defined \$R_t\$

This is generated using the `example_ip()` infectivity profile

## Usage

``` r
test_poisson_rt()
```

## Value

A dataframe containing the following columns:

- time (as.time_period) - the time column

- rt (numeric) - the time varying rt column (parameters)

- imports (numeric) - the imports column

- rate (numeric) - the poisson rate column (underlying infection rate)

- count (integer) - the count column

- statistic (character) - the statistic column

81 rows and 6 columns
