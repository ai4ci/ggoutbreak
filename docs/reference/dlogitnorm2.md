# Logit-normal distribution

The logit-normal distribution has a support of 0 to 1.

## Usage

``` r
dlogitnorm2(x, prob.0.5 = 0.5, kappa = 1 - exp(-1), log = FALSE)
```

## Arguments

- x:

  vector of quantiles `(0<x<1)`

- prob.0.5:

  the median on the true scale

- kappa:

  a dispersion parameter from 0 (none) to 1 maximum dispersion

- log:

  logical; if TRUE, probabilities p are given as log(p).

## Value

a vector of probabilities, quantiles, densities or samples.

## Examples

``` r
q = seq(0.1,0.9,0.1)
eps = sqrt(.Machine$double.eps)
dlogitnorm2(q, 0.75, 0.2)
#> [1] 8.444866e-47 1.318563e-26 2.611608e-16 1.014980e-09 3.898403e-05
#> [6] 5.982202e-02 4.515133e+00 4.867271e+00 1.082890e-04

(plogitnorm2(q+eps, 0.75, 0.2) -
  plogitnorm2(q-eps, 0.75, 0.2)) / (2*eps)
#> [1] 8.444866e-47 1.318563e-26 2.611608e-16 1.014980e-09 3.898403e-05
#> [6] 5.982202e-02 4.515133e+00 4.867271e+00 1.082905e-04
```
