# A quantile function for a mixture of normal distributions

A quantile function for a mixture of normal distributions

## Usage

``` r
.qmixnorm(
  p,
  means,
  sds,
  weights = 1,
  na.rm = FALSE,
  method = c("exact", "samples", "moments"),
  ...
)
```

## Arguments

- p:

  vector of probabilities.

- means:

  a vector of normal distribution means

- sds:

  a vector of normal distribution sds

- weights:

  a vector of weights

- na.rm:

  remove distributions with NA values for mean or sd

- method:

  one of `exact` (solve with uniroot), `samples` (random resampling),
  `moments` (Cornish Fisher approximation)

- ...:

  passed on to internal function, `samples=TRUE` to force random
  sampling

## Value

the value of the `p`th quantile

## Examples

``` r
try({
.qmixnorm(p=c(0.025,0.5,0.975), means=c(10,13,14), sds=c(1,1,2), method="exact")
.qmixnorm(p=c(0.025,0.5,0.975), means=c(10,13,14), sds=c(1,1,2), method="moments")
})
#> Error in .qmixnorm(p = c(0.025, 0.5, 0.975), means = c(10, 13, 14), sds = c(1,  : 
#>   could not find function ".qmixnorm"
```
