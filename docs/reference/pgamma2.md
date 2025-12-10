# The Gamma Distribution

Density, distribution function, quantile function and random generation
for the Gamma distribution with parameters `shape` and `scale`.

## Usage

``` r
pgamma2(q, mean, sd = sqrt(mean), lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- q:

  vector of quantiles

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

[`stats::pgamma()`](https://rdrr.io/r/stats/GammaDist.html)

## Examples

``` r
pgamma2(seq(0,4,0.25), 2, 1)
#>  [1] 0.000000000 0.001751623 0.018988157 0.065642454 0.142876540 0.242423867
#>  [7] 0.352768111 0.463367332 0.566529880 0.657704044 0.734974085 0.798300801
#> [13] 0.848796117 0.888150388 0.918234584 0.940854540 0.957619888
```
