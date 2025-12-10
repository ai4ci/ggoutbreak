# The Beta Distribution

Density, distribution function, quantile function and random generation
for the Beta distribution with parameters `shape1` and `shape2` (and
optional non-centrality parameter `ncp`).

## Usage

``` r
pbeta2(q, prob, kappa, lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- q:

  vector of quantiles

- prob:

  the mean probability (vectorised)

- kappa:

  a coefficient of variation. where 0 is no variability and 1 is
  maximally variability (vectorised)

- lower.tail:

  logical; if TRUE (default), probabilities are `P[X<=x]` otherwise
  `P[X>x]`.

- log.p:

  logical; if TRUE, probabilities p are given as log(p).

## Value

`dbeta` gives the density, `pbeta` the distribution function, `qbeta`
the quantile function, and `rbeta` generates random deviates.

Invalid arguments will result in return value `NaN`, with a warning.

The length of the result is determined by `n` for `rbeta`, and is the
maximum of the lengths of the numerical arguments for the other
functions.

The numerical arguments other than `n` are recycled to the length of the
result. Only the first elements of the logical arguments are used.

## See also

[`stats::pbeta()`](https://rdrr.io/r/stats/Beta.html)

## Examples

``` r
pbeta2(c(0.25,0.5,0.75), 0.5, 0.25)
#> [1] 0.0001270637 0.5000000000 0.9998729363
```
