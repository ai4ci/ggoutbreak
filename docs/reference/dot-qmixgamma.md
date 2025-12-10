# A quantile function for a mixture of gamma distributions

A quantile function for a mixture of gamma distributions

## Usage

``` r
.qmixgamma(
  p,
  shapes,
  rates,
  weights = 1,
  na.rm = FALSE,
  method = c("exact", "samples", "moments"),
  ...
)
```

## Arguments

- p:

  vector of probabilities.

- shapes:

  a vector of gamma distribution shapes

- rates:

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
  .qmixgamma(p=c(0.025,0.5,0.975), shapes=c(10,13,14), rates=c(1,1,2), method="moments")
  .qmixgamma(p=c(0.025,0.5,0.975), shapes=c(10,13,14), rates=c(1,1,2), method="exact")

  means = runif(100,5,6)
  sds = runif(100,2,3)
  shapes = means^2/sds^2
  rates = means/sds^2

  if (sd(means) < mean(sds)) message("mixture moments should be close")

  qgamma(c(0.025,0.5,0.975), shape =mean(means^2)/mean(sds^2), rate = mean(means)/mean(sds^2))
  system.time(
  .qmixgamma(p=c(0.025,0.5,0.975), shapes=shapes, rates=rates, method="moments")
  )
  system.time(
  .qmixgamma(p=c(0.025,0.5,0.975), shapes=shapes, rates=rates, method="exact")
  )

  means = runif(100,2,12)
  sds = runif(100,0.5,1)
  shapes = means^2/sds^2
  rates = means/sds^2

  if (sd(means) < mean(sds)) message("mixture moments should be close")

  qgamma(c(0.025,0.5,0.975), shape =mean(means^2)/mean(sds^2), rate = mean(means)/mean(sds^2))
  system.time(
  .qmixgamma(p=c(0.025,0.5,0.975), shapes=shapes, rates=rates, method="moments")
  )
  system.time(
  .qmixgamma(p=c(0.025,0.5,0.975), shapes=shapes, rates=rates, method="exact")
  )
  quantile(means, prob=c(0.025,0.5,0.975))

})
#> Error in .qmixgamma(p = c(0.025, 0.5, 0.975), shapes = c(10, 13, 14),  : 
#>   could not find function ".qmixgamma"
```
