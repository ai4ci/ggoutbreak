# The cumulative density function of a mixture of gamma distributions

The cumulative density function of a mixture of gamma distributions

## Usage

``` r
.pmixgamma(q, shapes, rates, weights = 1, na.rm = FALSE)
```

## Arguments

- q:

  vector of quantiles.

- shapes:

  a vector of gamma distribution shapes

- rates:

  a vector of gamma distribution rates

- weights:

  a vector of weights

- na.rm:

  remove distributions which have NA for shape or rate

## Value

the pdf of the mixture distribution.

## Examples

``` r
try({
  .pmixgamma(q=c(2,20), shapes=c(10,13,14), rates=c(1,1,1), weights=c(2,2,3))


})
#> Error in .pmixgamma(q = c(2, 20), shapes = c(10, 13, 14), rates = c(1,  : 
#>   could not find function ".pmixgamma"
```
