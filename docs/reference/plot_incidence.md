# Plot an incidence timeseries

Plot an incidence timeseries

## Usage

``` r
plot_incidence(
  modelled,
  raw = i_incidence_data,
  ...,
  mapping = .check_for_aes(modelled, ...),
  events = i_events
)
```

## Arguments

- modelled:

  An optional estimate of the incidence time series. If `modelled` is
  missing then it is estimated from `raw` using a
  `poisson_locfit_model`. In this case parameters `window` and `deg` may
  be supplied to control the fit. `modelled` can also be the output from
  `normalise_incidence` in which case the plot uses the per capita rates
  calculated by that function. - EITHER: a dataframe with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - incidence.per_capita.fit (double) - an estimate of the incidence per
    capita rate on a log scale

  - incidence.per_capita.se.fit (positive_double) - the standard error
    of the incidence per capita rate estimate on a log scale

  - incidence.per_capita.0.025 (positive_double) - lower confidence
    limit of the incidence per capita rate (true scale)

  - incidence.per_capita.0.5 (positive_double) - median estimate of the
    incidence per capita rate (true scale)

  - incidence.per_capita.0.975 (positive_double) - upper confidence
    limit of the incidence per capita rate (true scale)

  - population_unit (double) - The population unit on which the per
    capita incidence rate is calculated

  - time_unit (lubridate::as.period) - The time period over which the
    per capita incidence rate is calculated

  Any grouping allowed.

  OR with columns:

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

- raw:

  The raw count data (optional - if given overlaid as points on top of
  modelled estimate) - a dataframe with columns:

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

- ...:

  Named arguments passed on to
  [`geom_events`](https://ai4ci.github.io/ggoutbreak/reference/geom_events.md)

  `events`

  :   Significant events or time spans - a dataframe with columns:

      - label (character) - the event label

      - start (date) - the start date, or the date of the event

      - end (date) - the end date or NA if a single event

      Any grouping allowed.

      A default value is defined.

  Named arguments passed on to
  [`poisson_locfit_model`](https://ai4ci.github.io/ggoutbreak/reference/poisson_locfit_model.md)

  `window`

  :   a number of data points defining the bandwidth of the estimate,
      smaller values result in less smoothing, large value in more. The
      default value of 14 is calibrated for data provided on a daily
      frequency, with weekly data a lower value may be preferred. -
      default `14`

  `deg`

  :   polynomial degree (min 1) - higher degree results in less
      smoothing, lower values result in more smoothing. A degree of 1 is
      fitting a linear model piece wise. - default `2`

  `frequency`

  :   the density of the output estimates as a time period such as
      `7 days` or `2 weeks`. - default `1 day`

  Named arguments passed on to
  [`geom_truth`](https://ai4ci.github.io/ggoutbreak/reference/geom_truth.md)

  `true_df`

  :   a data frame with a `time_period` column called time and a value
      column (name given in `true_col`). This is optional and will be
      picked up from the `raw` parameter if given.

  `true_col`

  :   the column name / expression of the true value

  `true_fmt`

  :   a list of ggplot formatting to apply to the true value timeseries

  `...`

  :   Named arguments passed on to
      [`as.time_period`](https://ai4ci.github.io/ggoutbreak/reference/as.time_period.md)

      `unit`

      :   the length of one unit of time. This will be either a integer
          number of days, or a specification such as "1 week", or
          another `time_period`. If `x` is a `time_period`, and the unit
          is different to that of `x` this will return a rescaled
          `time_period` using the new units.

      `start_date`

      :   the zero time date as something that can be coerced to a date.
          If the `x` input is already a `time_period` and this is
          different to its `start_date` then `x` will be recalibrated to
          use the new start date.

      `anchor`

      :   only relevant if `x` is a vector of dates, this is a date, or
          `"start"` or `"end"` or a weekday name e.g. `"mon"`. With the
          vector of dates in `x` it will use this anchor to find a
          reference date for the time-series. If not provided then the
          current defaults will be used. (see
          [`set_defaults()`](https://ai4ci.github.io/ggoutbreak/reference/set_defaults.md))

- mapping:

  a [`ggplot2::aes`](https://ggplot2.tidyverse.org/reference/aes.html)
  mapping. Most importantly setting the `colour` to something if there
  are multiple incidence timeseries in the plot

- events:

  Significant events or time spans - a dataframe with columns:

  - label (character) - the event label

  - start (date) - the start date, or the date of the event

  - end (date) - the end date or NA if a single event

  Any grouping allowed.

  A default value is defined.

## Value

a ggplot object

## Examples

``` r
# example code

tmp = example_poisson_rt_2class()
tmp2 = tmp %>% poisson_locfit_model()

if(interactive()) {
  plot_incidence(tmp2,tmp,size=0.25)
}
```
