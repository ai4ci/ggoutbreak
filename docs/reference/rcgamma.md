# Sampling: gamma distribution constrained to have mean \> sd

The following conversion describes the parameters mean and kappa

## Usage

``` r
rcgamma(n, mean, kappa = 1/mean)
```

## Arguments

- n:

  number of observations

- mean:

  the mean value on the true scale (vectorised)

- kappa:

  a coefficient of variation. where 0 is no variability and 1 is
  maximally variability (vectorised)

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

\$\$ \text{shape:} \alpha = \frac{1}{\kappa} \\ \text{rate:} \beta =
\frac{1}{\mu \times \kappa} \\ \text{scale:} \sigma = \mu \times \kappa
\\ \$\$

## See also

[`stats::rgamma()`](https://rdrr.io/r/stats/GammaDist.html)

## Examples

``` r
rcgamma(10, 2, 0.5)
#>  [1] 0.8583552 2.0635676 2.2472948 1.6251910 1.1314753 2.9139310 1.4479930
#>  [8] 4.2213040 2.2316691 2.3661440
```
