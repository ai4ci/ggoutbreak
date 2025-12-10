# The Beta Distribution

Density, distribution function, quantile function and random generation
for the Beta distribution with parameters `shape1` and `shape2` (and
optional non-centrality parameter `ncp`).

## Usage

``` r
dbeta2(x, prob, kappa, log = FALSE)
```

## Arguments

- x:

  vector of quantiles

- prob:

  the mean probability (vectorised)

- kappa:

  a coefficient of variation. where 0 is no variability and 1 is
  maximally variability (vectorised)

- log:

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

[`stats::dbeta()`](https://rdrr.io/r/stats/Beta.html)

## Examples

``` r
dbeta2(c(0.25,0.5,0.75), 0.5, 0.25)
#> [1] 0.008405383 5.441004532 0.008405383
```
