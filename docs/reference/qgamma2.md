# The Gamma Distribution

Density, distribution function, quantile function and random generation
for the Gamma distribution with parameters `shape` and `scale`.

## Usage

``` r
qgamma2(p, mean, sd = sqrt(mean), lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- p:

  vector of probabilities

- mean:

  the mean value on the true scale (vectorised)

- sd:

  the standard deviation on the true scale (vectorised)

- lower.tail:

  logical; if TRUE (default), probabilities are `P[X<=x]` otherwise
  `P[X>x]`.

- log.p:

  logical; if TRUE, probabilities p are given as log(p).

## Value

`dgamma` gives the density, `pgamma` gives the distribution function,
`qgamma` gives the quantile function, and `rgamma` generates random
deviates.

Invalid arguments will result in return value `NaN`, with a warning.

The length of the result is determined by `n` for `rgamma`, and is the
maximum of the lengths of the numerical arguments for the other
functions.

The numerical arguments other than `n` are recycled to the length of the
result. Only the first elements of the logical arguments are used.

## See also

[`stats::qgamma()`](https://rdrr.io/r/stats/GammaDist.html)

## Examples

``` r
qgamma2(c(0.25,0.5,0.75), 2, 1)
#> [1] 1.267660 1.836030 2.554714
```
