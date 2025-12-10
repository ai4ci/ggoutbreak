# Logit-normal distribution

The logit-normal distribution has a support of 0 to 1.

## Usage

``` r
rlogitnorm2(n, prob.0.5 = 0.5, kappa = 1 - exp(-1))
```

## Arguments

- n:

  number of observations

- prob.0.5:

  the median on the true scale

- kappa:

  a dispersion parameter from 0 (none) to 1 maximum dispersion

## Value

a vector of probabilities, quantiles, densities or samples.

## Examples

``` r
mean(rlogitnorm2(10000,0.75,0.2))
#> [1] 0.7478397
```
