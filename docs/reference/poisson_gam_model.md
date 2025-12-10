# GAM poisson time-series model

This function lets the user supply a fitting function that models
incidence, and provides a set of machinery for applying it to groups,
extracting incidence, growth rates, and optionally reproduction numbers
from the fit(s). This is more advanced than other estimators as there
are far more configuration of GAM models, so this aims to provide
sensible defaults with the option to bring your own model.

## Usage

``` r
poisson_gam_model(
  d,
  ...,
  frequency = "1 day",
  ip = i_discrete_ip,
  quick = FALSE,
  .progress = interactive()
)
```

## Arguments

- d:

  input data - EITHER: a dataframe with columns:

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - obs_time (ggoutbreak::time_period) - The time of the observation of
    the time series, as a \`time_period\`

  Minimally grouped by: obs_time (and other groupings allowed).

  OR with columns:

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

- ...:

  Named arguments passed on to
  [`poisson_gam_model.censored`](https://ai4ci.github.io/ggoutbreak/reference/poisson_gam_model.censored.md)

  `model_fn`

  :   a function that takes data relating to one time series (e.g. the
      input data `d` on a group by group basis) and returns a fitted
      GAM. The default is creates a delayed reporting model
      [`gam_delayed_reporting()`](https://ai4ci.github.io/ggoutbreak/reference/gam_delayed_reporting.md).

  `predict`

  :   if the GAM model in `model_fn` introduces other variables we need
      to know what their values should be fixed at for prediction. This
      is a named list of defaults for variables in the model supplied by
      `model_fn`. These defaults will be used in prediction. This may be
      supplied as part of the model function generator ( e.g.
      `gam_delayed_reporting(...)$predict`). If this is set to exactly
      `FALSE` no prediction is performed and a list column of fitted GAM
      models returned instead.

  Named arguments passed on to
  [`poisson_gam_model.incidence`](https://ai4ci.github.io/ggoutbreak/reference/poisson_gam_model.incidence.md)

  `model_fn`

  :   a function that takes data relating to one time series (e.g. the
      input data `d` on a group by group basis) and returns a fitted
      GAM. The default creates a simple poisson model based on count
      alone
      ([`gam_poisson_model_fn()`](https://ai4ci.github.io/ggoutbreak/reference/gam_poisson_model_fn.md)).

  `predict`

  :   if the GAM model in `model_fn` introduces other variables we need
      to know what their values should be fixed at for prediction. This
      is a named list of defaults for variables in the model supplied by
      `model_fn`. These defaults will be used in prediction. This may be
      supplied as part of the model function generator ( e.g.
      `gam_delayed_reporting(...)$predict`). If this is set to exactly
      `FALSE` no prediction is performed and a list column of fitted GAM
      models returned instead.

- frequency:

  the density of the output estimates as a time period such as `7 days`
  or `2 weeks`.

- ip:

  An infectivity profile (optional) if not given (the default) the Rt
  value will not be estimated - a dataframe with columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - tau (integer + complete) - the days since the index event.

  Minimally grouped by: boot (and other groupings allowed).

- quick:

  if `ip` is provided, and quick is `TRUE` Rt estimation will be done
  assuming independence which is quicker but less accurate. Setting this
  to false will use a full variance-covariance matrix.

- .progress:

  show a CLI progress bar

## Value

A dataframe containing the following columns:

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

- incidence.fit (double) - an estimate of the incidence rate on a log
  scale

- incidence.se.fit (positive_double) - the standard error of the
  incidence rate estimate on a log scale

- incidence.0.025 (positive_double) - lower confidence limit of the
  incidence rate (true scale)

- incidence.0.5 (positive_double) - median estimate of the incidence
  rate (true scale)

- incidence.0.975 (positive_double) - upper confidence limit of the
  incidence rate (true scale)

- growth.fit (double) - an estimate of the growth rate

- growth.se.fit (positive_double) - the standard error the growth rate

- growth.0.025 (double) - lower confidence limit of the growth rate

- growth.0.5 (double) - median estimate of the growth rate

- growth.0.975 (double) - upper confidence limit of the growth rate

Any grouping allowed.

additionally if `ip` is given: A dataframe containing the following
columns:

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

- rt.fit (double) - an estimate of the reproduction number

- rt.se.fit (positive_double) - the standard error of the reproduction
  number

- rt.0.025 (double) - lower confidence limit of the reproduction number

- rt.0.5 (double) - median estimate of the reproduction number

- rt.0.975 (double) - upper confidence limit of the reproduction number

Any grouping allowed.

## Examples

``` r
# Simple poisson model
data = example_poisson_rt_smooth()
tmp2 = poisson_gam_model(data,window=7,ip=example_ip(),quick=TRUE)
#> Rt estimation using GAM (approx and assuming independence)

if (interactive()) {
  plot_incidence(
    tmp2,
    date_labels="%b %y",
    raw=data
  )

  plot_rt(
    tmp2,
    date_labels="%b %y"
  )+
  sim_geom_function(data,colour="red")
}



# example with delayed observation model.
# This data is all the day by day observations of the whole timeseries from
# the beginning of the outbreak.
data2 = example_delayed_observation()
model = gam_delayed_reporting(window = 14)
tmp3 = data2 %>% poisson_gam_model(
  model_fn = model$model_fn,
  predict = model$predict,
  ip=example_ip())
#> Rt estimation using GAM (exact with modelled covariance)

if (interactive()) {
  plot_incidence(tmp3)+
    ggplot2::geom_line(
      data=data2 %>% dplyr::filter(obs_time %% 10 == 0),
      mapping = ggplot2::aes(x=as.Date(time),y=count,colour=as.factor(obs_time))
    )

  plot_rt(tmp3)
}
```
