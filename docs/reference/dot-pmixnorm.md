# The cumulative density function of a mixture of normal distributions

The cumulative density function of a mixture of normal distributions

## Usage

``` r
.pmixnorm(q, means, sds, weights = 1, na.rm = FALSE)
```

## Arguments

- q:

  vector of quantiles.

- means:

  a vector of normal distribution means

- sds:

  a vector of normal distribution sds

- weights:

  a vector of weights

- na.rm:

  remove distributions which have NA for mean or sd

## Value

the pdf of the mixture distribution.

## Examples

``` r
try({
.pmixnorm(q=c(2,20), means=c(10,13,14), sds=c(1,1,2), weights=c(2,2,3))
})
#> Error in .pmixnorm(q = c(2, 20), means = c(10, 13, 14), sds = c(1, 1,  : 
#>   could not find function ".pmixnorm"
```
