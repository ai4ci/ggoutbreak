# The cumulative density function of a mixture of beta distributions

The cumulative density function of a mixture of beta distributions

## Usage

``` r
.pmixbeta(q, alphas, betas, weights = 1, na.rm = FALSE)
```

## Arguments

- q:

  vector of quantiles.

- alphas:

  a vector of beta distribution alphas

- betas:

  a vector of gamma distribution betas

- weights:

  a vector of weights

- na.rm:

  remove distributions which have NA for shape or rate

## Value

the pdf of the mixture distribution.

## Examples

``` r
try({
  .pmixbeta(q=c(2,20), alphas=c(10,13,14), betas=c(1,1,1), weights=c(2,2,3))
})
#> Error in .pmixbeta(q = c(2, 20), alphas = c(10, 13, 14), betas = c(1,  : 
#>   could not find function ".pmixbeta"
```
