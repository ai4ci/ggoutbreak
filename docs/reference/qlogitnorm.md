# Logit-normal distribution

The logit-normal distribution has a support of 0 to 1.

## Usage

``` r
qlogitnorm(p, meanlogit = 0, sdlogit = 1, lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- p:

  vector of probabilities

- meanlogit:

  the mean on the logit scale

- sdlogit:

  the sd on the logit scale

- lower.tail:

  logical; if TRUE (default), probabilities are `P[X<=x]` otherwise
  `P[X>x]`.

- log.p:

  logical; if TRUE, probabilities p are given as log(p).

## Value

a vector of probabilities, quantiles, densities or samples.

## Examples

``` r
qlogitnorm(c(0.25,0.5,0.75), 0, 1)
#> [1] 0.3374922 0.5000000 0.6625078
```
