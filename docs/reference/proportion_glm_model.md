# Binomial time-series model.

This uses a generalised linear model to fit a quasi-binomial model with
a time varying rate as a natural cubic spline with approx one degree of
freedom per `window` units of the time series.

## Usage

``` r
proportion_glm_model(
  d = i_proportion_input,
  ...,
  window = 14,
  frequency = "1 day",
  .progress = interactive()
)
```

## Arguments

- d:

  Proportion model input - a dataframe with columns:

  - denom (positive_integer) - Total test counts associated with the
    specified time frame

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

- ...:

  not used and present to allow proportion model to be used in a
  `group_modify`

- window:

  a number of data points defining the bandwidth of the estimate,
  smaller values result in less smoothing, large value in more. The
  default value of 14 is calibrated for data provided on a daily
  frequency, with weekly data a lower value may be preferred. - default
  `14`

- frequency:

  the density of the output estimates as a time period such as `7 days`
  or `2 weeks`. - default `1 day`

- .progress:

  show a CLI progress bar

## Value

A dataframe containing the following columns:

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

- proportion.fit (double) - an estimate of the proportion on a logit
  scale

- proportion.se.fit (positive_double) - the standard error of proportion
  estimate on a logit scale

- proportion.0.025 (proportion) - lower confidence limit of proportion
  (true scale)

- proportion.0.5 (proportion) - median estimate of proportion (true
  scale)

- proportion.0.975 (proportion) - upper confidence limit of proportion
  (true scale)

Any grouping allowed.

## Examples

``` r
data = example_poisson_rt_2class()

tmp2 = data %>% proportion_glm_model(window=7,deg=2)
tmp3 = data %>% proportion_glm_model(window=14,deg=1)

comp = dplyr::bind_rows(
  tmp2 %>% dplyr::mutate(model="7:2"),
  tmp3 %>% dplyr::mutate(model="14:1"),
) %>% dplyr::group_by(model,class)

if (interactive()) {
  plot_proportion(
      comp,
      date_labels="%b %y",
      mapping=ggplot2::aes(colour=model),
      raw=data
   )+
   ggplot2::facet_wrap(~class)
}

# TODO: deal with error conditions
# "observations with zero weight not used for calculating dispersion
```
