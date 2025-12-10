# A quantile function for a mixture of log normal distributions

A quantile function for a mixture of log normal distributions

## Usage

``` r
.qmixlnorm(
  p,
  meanlogs,
  sdlogs,
  weights = rep(1, length(meanlogs)),
  na.rm = FALSE,
  method = c("exact", "samples", "moments"),
  ...
)
```

## Arguments

- p:

  vector of probabilities.

- meanlogs:

  a vector of log normal distribution means

- sdlogs:

  a vector of log normal distribution sds

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
  .qmixlnorm(p=c(0.025,0.5,0.975), meanlogs=c(1,1.3,1.4), sdlogs=c(0.1,0.1,0.2))
  .qmixlnorm(p=c(0.025,0.5,0.975), meanlogs=c(1,1.3,1.4), sdlogs=c(0.1,0.1,0.2), method="samples")
  .qmixlnorm(p=c(0.025,0.5,0.975), meanlogs=c(1,1.3,1.4), sdlogs=c(0.1,0.1,0.2), method="moments")
})
#> Error in .qmixlnorm(p = c(0.025, 0.5, 0.975), meanlogs = c(1, 1.3, 1.4),  : 
#>   could not find function ".qmixlnorm"
```
