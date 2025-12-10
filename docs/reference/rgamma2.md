# The Gamma Distribution

Density, distribution function, quantile function and random generation
for the Gamma distribution with parameters `shape` and `scale`.

## Usage

``` r
rgamma2(n, mean, sd = sqrt(mean))
```

## Arguments

- n:

  number of observations

- mean:

  the mean value on the true scale (vectorised)

- sd:

  the standard deviation on the true scale (vectorised)

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

[`stats::rgamma()`](https://rdrr.io/r/stats/GammaDist.html)

## Examples

``` r
rgamma2(10, 2, 1)
#>  [1] 1.0862935 2.2233210 1.6541772 1.0078365 2.1321339 3.1695478 0.7653657
#>  [8] 3.0183174 3.9632970 1.4411757
```
