# Cumulative probability: gamma distribution constrained to have mean \> sd

The following conversion describes the parameters mean and kappa

## Usage

``` r
pcgamma(q, mean, kappa = 1/mean, lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- q:

  vector of quantiles

- mean:

  the mean value on the true scale (vectorised)

- kappa:

  a coefficient of variation. where 0 is no variability and 1 is
  maximally variability (vectorised)

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

## Details

\$\$ \text{shape:} \alpha = \frac{1}{\kappa} \\ \text{rate:} \beta =
\frac{1}{\mu \times \kappa} \\ \text{scale:} \sigma = \mu \times \kappa
\\ \$\$

## See also

[`stats::pgamma()`](https://rdrr.io/r/stats/GammaDist.html)

## Examples

``` r
pcgamma(seq(0,4,0.25), 2, 0.5)
#>  [1] 0.00000000 0.02649902 0.09020401 0.17335853 0.26424112 0.35536421
#>  [7] 0.44217460 0.52212166 0.59399415 0.65745252 0.71270250 0.76027052
#> [13] 0.80085173 0.83520962 0.86411177 0.88829071 0.90842181
```
