# Logit-normal distribution

The logit-normal distribution has a support of 0 to 1.

## Arguments

- n:

  number of observations

- x:

  vector of quantiles `(0<x<1)`

- q:

  vector of quantiles `(0<q<1)`

- p:

  vector of probabilities

- log:

  logical; if TRUE, probabilities p are given as log(p).

- log.p:

  logical; if TRUE, probabilities p are given as log(p).

- lower.tail:

  logical; if TRUE (default), probabilities are `P[X<=x]` otherwise
  `P[X>x]`.

- meanlogit:

  the mean on the logit scale

- sdlogit:

  the sd on the logit scale

- prob.0.5:

  the median on the true scale

- kappa:

  a dispersion parameter from 0 (none) to 1 maximum dispersion

## Value

a vector of probabilities, quantiles, densities or samples.
