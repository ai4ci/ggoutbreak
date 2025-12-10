# Reproduction number from modelled incidence

Calculate a reproduction number estimate from modelled incidence using
the methods described in the vignette "Estimating the reproduction
number from modelled incidence" and using a set of empirical generation
time distributions. This assumes that modelled incidence has the same
time unit as the `ip` distribution, and that this is daily, if this is
not the case then
[`rescale_model()`](https://ai4ci.github.io/ggoutbreak/reference/rescale_model.md)
may be able to fix it.

## Usage

``` r
rt_from_incidence(
  df = i_incidence_model,
  ip = i_discrete_ip,
  raw = i_incidence_data,
  approx = TRUE,
  .progress = interactive()
)
```

## Arguments

- df:

  modelled incidence estimate - a dataframe with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

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

  Any grouping allowed.

- ip:

  an infectivity profile (aka generation time distribution) - a
  dataframe with columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - tau (integer + complete) - the days since the index event.

  Minimally grouped by: boot (and other groupings allowed).

- raw:

  the raw data that the modelled incidence is based on. This is
  optional. If not given the algorithm will assume independence, which
  will be faster to estimate (much faster for long time-series), but
  with more uncertain Rt estimates. In some circumstances this
  assumption of independence can cause underestimation of Rt. If this is
  a risk there will be a warning given, and this parameter may need to
  be supplied. - a dataframe with columns:

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

- approx:

  use a faster, but approximate, estimate of quantiles

- .progress:

  show a CLI progress bar

## Value

A dataframe containing the following columns:

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

- rt.fit (double) - an estimate of the reproduction number

- rt.se.fit (positive_double) - the standard error of the reproduction
  number

- rt.0.025 (double) - lower confidence limit of the reproduction number

- rt.0.5 (double) - median estimate of the reproduction number

- rt.0.975 (double) - upper confidence limit of the reproduction number

Any grouping allowed.

## Details

N.B. for certain estimators (e.g.
[`poisson_gam_model()`](https://ai4ci.github.io/ggoutbreak/reference/poisson_gam_model.md),
[`poisson_locfit_model()`](https://ai4ci.github.io/ggoutbreak/reference/poisson_locfit_model.md))
a version of this function will be called automatically if the
infectivity profile is supplied. The inbuilt version is preferred over
this function for these estimators, as the full covariance matrix may be
used and the initial part of the outbreak can be predicted more
accurately. This function is for supporting the other count models in
`ggoutbreak`. If you are rolling your own incidence estimates and want
an Rt estimate then
[`rt_incidence_timeseries_implementation()`](https://ai4ci.github.io/ggoutbreak/reference/rt_incidence_timeseries_implementation.md)
maybe better suited to your task.

## Examples

``` r
tmp = example_poisson_rt_smooth() %>%
  poisson_locfit_model() %>%
  rt_from_incidence(
    ip = example_ip(),
    raw = example_poisson_rt_smooth(),
    approx=FALSE
 )
#> Rt from incidence: inferring vcov from residuals. 
#> (N.B. this message will only be displayed once.)

# This will assume independence and
tmp2 = example_poisson_rt_smooth()%>%
  poisson_locfit_model() %>%
  rt_from_incidence(ip = example_ip(), approx=TRUE)
#> Estimates were assumed to be independent, but more that 1% of estimates
#> are at risk of Rt underestimation by more that 0.05 (absolute).
#> We advise re-running supplying a full variance-covariance matrix, or
#> a value to the `raw` parameter, or setting `quick=FALSE`. 
#> (N.B. this message will only be displayed once.)

plot_data = dplyr::bind_rows(
  tmp %>% dplyr::mutate(class = "exact"),
  tmp2 %>% dplyr::mutate(class = "approx"),
) %>% dplyr::group_by(class)

if (interactive()) {
  plot_rt(plot_data, date_labels="%b %y")+
   sim_geom_function(example_poisson_rt_smooth())+
   ggplot2::coord_cartesian(ylim=c(0.5,3))+
   ggplot2::facet_wrap(~class)
}
```
