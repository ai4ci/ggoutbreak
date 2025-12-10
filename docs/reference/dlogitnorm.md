# Logit-normal distribution

The logit-normal distribution has a support of 0 to 1.

## Usage

``` r
dlogitnorm(x, meanlogit = 0, sdlogit = 1, log = FALSE)
```

## Arguments

- x:

  vector of quantiles `(0<x<1)`

- meanlogit:

  the mean on the logit scale

- sdlogit:

  the sd on the logit scale

- log:

  logical; if TRUE, probabilities p are given as log(p).

## Value

a vector of probabilities, quantiles, densities or samples.

## Examples

``` r
dlogitnorm(seq(0.1,0.9,0.1), 0, 1)
#> [1] 0.3965747 0.9538364 1.3267766 1.5310853 1.5957691 1.5310853 1.3267766
#> [8] 0.9538364 0.3965747
```
