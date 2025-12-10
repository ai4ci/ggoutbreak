# Delayed GAM reporting model function generator

Delayed GAM reporting model function generator

## Usage

``` r
gam_delayed_reporting(
  window,
  max_delay = 40,
  ...,
  knots_fn = ~gam_knots(.x, window, ...)
)
```

## Arguments

- window:

  controls the knot spacing in the GAM (if the default)

- max_delay:

  the maximum delay we expect to model

- ...:

  Named arguments passed on to
  [`gam_knots`](https://ai4ci.github.io/ggoutbreak/reference/gam_knots.md)

  `k`

  :   alternative to `window`, if `k` is given then the behaviour of the
      knots will be similar to the default `mgcv::s(...,k=...)`
      parameter.

- knots_fn:

  a function that takes the data as an input and returns a set of
  integers as time points for GAM knots, for `s(time)` term. The default
  here provides a roughly equally spaced grid determined by `window`, by
  a user supplied function could do anything. The input this function is
  the raw dataframe of data that will be considered for one model fit.
  It is guaranteed to have at least a `time` and `count` column. It is
  possible to

## Value

a list with 2 entries - `model_fn` and `predict` suitable as the input
for `poisson_gam_model(model_fn = ..., predict=...)`.

## Details

This function is used to configure a delayed reporting GAM model. The
model is of the form:

`count ~ s(time, bs = "cr", k = length(kts)) + s(log(tau), k = 4, pc = max_delay)`

where `tau` is the difference between time series observation time and
the time of the data point in the time series, and we have multiple
observations of the same time series. This function helps specify the
knots of the GAM and the maximum expected delay

## Examples

``` r
data = example_delayed_observation() %>% dplyr::group_by(obs_time)
cfg = gam_delayed_reporting(14,40)
fit = cfg$model_fn(data)
summary(fit)
#> 
#> Family: Negative Binomial(1561106.316) 
#> Link function: log 
#> 
#> Formula:
#> count ~ s(time, bs = "cr", k = length(kts)) + s(log(tau), k = 4, 
#>     pc = max_delay)
#> 
#> Parametric coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  1.62065    0.02503   64.75   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df Chi.sq p-value    
#> s(time)     6.962  6.999  48076  <2e-16 ***
#> s(log(tau)) 2.999  3.000   5510  <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> R-sq.(adj) =  0.984   Deviance explained = 98.4%
#> -REML = 6509.8  Scale est. = 1         n = 3240
```
