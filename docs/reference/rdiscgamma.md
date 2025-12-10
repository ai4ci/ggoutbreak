# Random count data from a discrete gamma distribution

For count data that is under-dispersed we can use a gamma distribution
rounded to the nearest whole number. This is the same as the method of
discretisation in `make_gamma_ip`, and suits delay distributions where
there is less variability than can be represented with a Poisson or
negative binomial distribution.

## Usage

``` r
rdiscgamma(n, mean, sd, kappa)
```

## Arguments

- n:

  number of observations

- mean:

  the mean value on the true scale (vectorised)

- sd:

  the standard deviation on the true scale (vectorised)

- kappa:

  a coefficient of variation. where 0 is no variability and 1 is
  maximally variability (vectorised)

## Value

an integer valued vector from a gamma distribution.

## See also

[`stats::rgamma()`](https://rdrr.io/r/stats/GammaDist.html)

## Examples

``` r
rdiscgamma(10, 2, 1)
#>  [1] 5 3 2 2 1 2 2 3 3 3
```
