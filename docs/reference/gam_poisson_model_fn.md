# Default GAM count model.

This function configures a very simple poisson count model and using:

## Usage

``` r
gam_poisson_model_fn(window, ..., knots_fn = ~gam_knots(.x, window, ...))
```

## Arguments

- window:

  controls the knot spacing in the GAM (if the default)

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

a function suitable as the input for
`poisson_gam_model(model_fn = ...)`.

## Details

`count ~ s(time, bs = "cr", k = length(kts))`

The control of knot positions is defined by the `knots_fn` and `window`
parameters.

## Examples

``` r
model_fn = gam_poisson_model_fn(14)
fit = model_fn(example_poisson_rt() %>% dplyr::ungroup())
summary(fit)
#> 
#> Family: quasipoisson 
#> Link function: log 
#> 
#> Formula:
#> count ~ s(time, bs = "cr", k = length(kts))
#> 
#> Parametric coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   7.1308     0.1036    68.8   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Approximate significance of smooth terms:
#>          edf Ref.df     F p-value    
#> s(time) 6.51  6.907 134.6  <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> R-sq.(adj) =  0.892   Deviance explained = 96.7%
#> -REML = 251.45  Scale est. = 134.47    n = 81
```
