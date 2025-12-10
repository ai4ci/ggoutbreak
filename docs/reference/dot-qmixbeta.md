# A quantile function for a mixture of gamma distributions

A quantile function for a mixture of gamma distributions

## Usage

``` r
.qmixbeta(
  p,
  alphas,
  betas,
  weights = 1,
  na.rm = FALSE,
  method = c("exact", "samples", "moments"),
  ...
)
```

## Arguments

- p:

  vector of probabilities.

- alphas:

  a vector of gamma distribution shapes

- betas:

  a vector of gamma distribution rates

- weights:

  a vector of weights

- na.rm:

  remove distributions with NA values for mean or sd

- method:

  one of `exact` (solve with uniroot), `samples` (random resampling),
  `moments` (Cornish Fisher approximation)

- ...:

  passed on to internal function, `seed=XXX` to fix random seed

## Value

the value of the `p`th quantile

## Examples

``` r
try({
  .qmixbeta(p=c(0.025,0.5,0.975), alphas=c(10,13,14), betas=c(1,1,2))
  .qmixbeta(p=c(0.025,0.5,0.975), alphas=c(10,13,14), betas=c(1,1,2), method="moments")
})
#> Error in .qmixbeta(p = c(0.025, 0.5, 0.975), alphas = c(10, 13, 14), betas = c(1,  : 
#>   could not find function ".qmixbeta"
```
