# Logit-normal distribution

The logit-normal distribution has a support of 0 to 1.

## Usage

``` r
plogitnorm(q, meanlogit = 0, sdlogit = 1, lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- q:

  vector of quantiles `(0<q<1)`

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
plogitnorm(seq(0.1,0.9,0.1), 0, 1)
#> [1] 0.01400221 0.08282852 0.19841456 0.34256783 0.50000000 0.65743217 0.80158544
#> [8] 0.91717148 0.98599779
```
