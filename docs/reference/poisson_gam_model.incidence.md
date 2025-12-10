# Poisson model for incidence data

Poisson model for incidence data

## Usage

``` r
poisson_gam_model.incidence(
  d = i_incidence_input,
  model_fn = gam_poisson_model_fn(...),
  ...,
  frequency = "1 day",
  predict = list(),
  ip = i_discrete_ip,
  quick = FALSE,
  .progress = interactive()
)
```

## Arguments

- model_fn:

  a function that takes data relating to one time series (e.g. the input
  data `d` on a group by group basis) and returns a fitted GAM. The
  default creates a simple poisson model based on count alone
  ([`gam_poisson_model_fn()`](https://ai4ci.github.io/ggoutbreak/reference/gam_poisson_model_fn.md)).

- predict:

  if the GAM model in `model_fn` introduces other variables we need to
  know what their values should be fixed at for prediction. This is a
  named list of defaults for variables in the model supplied by
  `model_fn`. These defaults will be used in prediction. This may be
  supplied as part of the model function generator ( e.g.
  `gam_delayed_reporting(...)$predict`). If this is set to exactly
  `FALSE` no prediction is performed and a list column of fitted GAM
  models returned instead.
