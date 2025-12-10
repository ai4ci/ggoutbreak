# Poisson time-series model.

This uses a generalised linear model to fit a quasi-poisson model with a
time varying rate as a natural cubic spline with approx one degree of
freedom per `window` units of the time series.

## Usage

``` r
poisson_glm_model(
  d = i_incidence_input,
  ...,
  window = 14,
  frequency = "1 day",
  .progress = interactive()
)
```

## Arguments

- d:

  Count model input - a dataframe with columns:

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

## Examples

``` r
data = example_poisson_growth_rate()

tmp2 = data %>% poisson_glm_model(window=7,deg=2)
tmp3 = data %>% poisson_glm_model(window=14,deg=1)

comp = dplyr::bind_rows(
  tmp2 %>% dplyr::mutate(class="7:2"),
  tmp3 %>% dplyr::mutate(class="14:1"),
) %>% dplyr::group_by(class)

if (interactive()) {
  plot_incidence(comp, date_labels="%b %y", raw=data, true_col=rate)
}
```
