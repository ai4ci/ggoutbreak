# Logit-normal distribution

The logit-normal distribution has a support of 0 to 1.

## Usage

``` r
qlogitnorm2(
  p,
  prob.0.5 = 0.5,
  kappa = 1 - exp(-1),
  lower.tail = TRUE,
  log.p = FALSE
)
```

## Arguments

- p:

  vector of probabilities

- prob.0.5:

  the median on the true scale

- kappa:

  a dispersion parameter from 0 (none) to 1 maximum dispersion

- lower.tail:

  logical; if TRUE (default), probabilities are `P[X<=x]` otherwise
  `P[X>x]`.

- log.p:

  logical; if TRUE, probabilities p are given as log(p).

## Value

a vector of probabilities, quantiles, densities or samples.

## Examples

``` r
qlnorm2(c(0.25,0.5,0.72), 2, 1)
#> [1] 1.300774 1.788854 2.355843
```
