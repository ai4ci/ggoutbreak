# Density: gamma distribution constrained to have mean \> sd

The following conversion describes the parameters mean and kappa

## Usage

``` r
dcgamma(x, mean, kappa = 1/mean, log = FALSE)
```

## Arguments

- x:

  vector of quantiles

- mean:

  the mean value on the true scale (vectorised)

- kappa:

  a coefficient of variation. where 0 is no variability and 1 is
  maximally variability (vectorised)

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

## Details

\$\$ \text{shape: } \alpha = \frac{1}{\kappa} \\ \text{rate: } \beta =
\frac{1}{\mu \times \kappa} \\ \text{scale: } \sigma = \mu \times \kappa
\\ \$\$

## See also

[`stats::dgamma()`](https://rdrr.io/r/stats/GammaDist.html)

## Examples

``` r
dcgamma(seq(0,4,0.25), 2, 0.5)
#>  [1] 0.00000000 0.19470020 0.30326533 0.35427491 0.36787944 0.35813100
#>  [7] 0.33469524 0.30410440 0.27067057 0.23714826 0.20521250 0.17580162
#> [13] 0.14936121 0.12601618 0.10569084 0.08819155 0.07326256
```
