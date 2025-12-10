# Poisson model for censored data

Poisson model for censored data

## Usage

``` r
poisson_gam_model.censored(
  d = i_censored_incidence_data,
  model_fn = gam_delayed_reporting(...)$model_fn,
  ...,
  frequency = "1 day",
  predict = gam_delayed_reporting(...)$predict,
  ip = i_discrete_ip,
  quick = FALSE,
  .progress = interactive()
)
```

## Arguments

- model_fn:

  a function that takes data relating to one time series (e.g. the input
  data `d` on a group by group basis) and returns a fitted GAM. The
  default is creates a delayed reporting model
  [`gam_delayed_reporting()`](https://ai4ci.github.io/ggoutbreak/reference/gam_delayed_reporting.md).

- ...:

  Named arguments passed on to
  [`gam_delayed_reporting`](https://ai4ci.github.io/ggoutbreak/reference/gam_delayed_reporting.md)

  `window`

  :   controls the knot spacing in the GAM (if the default)

  `max_delay`

  :   the maximum delay we expect to model

  `knots_fn`

  :   a function that takes the data as an input and returns a set of
      integers as time points for GAM knots, for `s(time)` term. The
      default here provides a roughly equally spaced grid determined by
      `window`, by a user supplied function could do anything. The input
      this function is the raw dataframe of data that will be considered
      for one model fit. It is guaranteed to have at least a `time` and
      `count` column. It is possible to

- predict:

  if the GAM model in `model_fn` introduces other variables we need to
  know what their values should be fixed at for prediction. This is a
  named list of defaults for variables in the model supplied by
  `model_fn`. These defaults will be used in prediction. This may be
  supplied as part of the model function generator ( e.g.
  `gam_delayed_reporting(...)$predict`). If this is set to exactly
  `FALSE` no prediction is performed and a list column of fitted GAM
  models returned instead.
