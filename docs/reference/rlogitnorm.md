# Logit-normal distribution

The logit-normal distribution has a support of 0 to 1.

## Usage

``` r
rlogitnorm(n, meanlogit = 0, sdlogit = 1)
```

## Arguments

- n:

  number of observations

- meanlogit:

  the mean on the logit scale

- sdlogit:

  the sd on the logit scale

## Value

a vector of probabilities, quantiles, densities or samples.

## Examples

``` r
rlogitnorm(10, 0, 1)
#>  [1] 0.60611671 0.21137710 0.54511759 0.68739382 0.71449537 0.48584213
#>  [7] 0.66074007 0.52711086 0.07422444 0.61497256
```
