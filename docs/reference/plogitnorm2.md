# Logit-normal distribution

The logit-normal distribution has a support of 0 to 1.

## Usage

``` r
plogitnorm2(
  q,
  prob.0.5 = 0.5,
  kappa = 1 - exp(-1),
  lower.tail = TRUE,
  log.p = FALSE
)
```

## Arguments

- q:

  vector of quantiles `(0<q<1)`

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
plogitnorm2(seq(0.1,0.9,0.1), 0.75, 0.2)
#> [1] 1.143062e-49 4.194162e-29 1.385603e-18 7.897412e-12 4.253902e-07
#> [6] 9.472742e-04 1.300308e-01 9.013399e-01 9.999996e-01
plogitnorm2(qlogitnorm2(seq(0.1,0.9,0.1), 0.75, 0.2), 0.75, 0.2)
#> [1] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
```
