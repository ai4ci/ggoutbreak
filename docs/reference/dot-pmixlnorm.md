# The cumulative density function of a mixture of log normal distributions

The cumulative density function of a mixture of log normal distributions

## Usage

``` r
.pmixlnorm(q, meanlogs, sdlogs, weights = 1, na.rm = FALSE)
```

## Arguments

- q:

  vector of quantiles.

- meanlogs:

  a vector of normal distribution means

- sdlogs:

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
.pmixlnorm(q=c(2,20), meanlogs=c(1.0,1.3,1.4), sdlogs=c(1,1,2), weights=c(2,2,3))
})
#> Error in .pmixlnorm(q = c(2, 20), meanlogs = c(1, 1.3, 1.4), sdlogs = c(1,  : 
#>   could not find function ".pmixlnorm"
```
