# The Beta Distribution

Density, distribution function, quantile function and random generation
for the Beta distribution with parameters `shape1` and `shape2` (and
optional non-centrality parameter `ncp`).

## Usage

``` r
rbeta2(n, prob, kappa)
```

## Arguments

- n:

  number of observations

- prob:

  the mean probability (vectorised)

- kappa:

  a coefficient of variation. where 0 is no variability and 1 is
  maximally variability (vectorised)

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

[`stats::rbeta()`](https://rdrr.io/r/stats/Beta.html)

## Examples

``` r
rbeta2(3, c(0.1,0.5,0.9),0.1)
#> [1] 0.1165689 0.5079711 0.9029945
```
