# The Gamma Distribution

Density, distribution function, quantile function and random generation
for the Gamma distribution with parameters `shape` and `scale`.

## Usage

``` r
dgamma2(x, mean, sd = sqrt(mean), log = FALSE)
```

## Arguments

- x:

  vector of quantiles

- mean:

  the mean value on the true scale (vectorised)

- sd:

  the standard deviation on the true scale (vectorised)

- log:

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

[`stats::dgamma()`](https://rdrr.io/r/stats/GammaDist.html)

## Examples

``` r
dgamma2(seq(0,4,0.25), 2, 1)
#>  [1] 0.00000000 0.02527211 0.12262648 0.25102143 0.36089409 0.42752603
#>  [7] 0.44808362 0.43157094 0.39073363 0.33743577 0.28074779 0.22664553
#> [13] 0.17847016 0.13762733 0.10425850 0.07777749 0.05725229
```
