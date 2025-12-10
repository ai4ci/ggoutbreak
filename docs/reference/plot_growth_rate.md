# Growth rate timeseries diagram

Growth rate timeseries diagram

## Usage

``` r
plot_growth_rate(
  modelled,
  ...,
  mapping = .check_for_aes(modelled, ...),
  events = i_events
)
```

## Arguments

- modelled:

  a growth rate dataframe. - EITHER: a dataframe with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - growth.fit (double) - an estimate of the growth rate

  - growth.se.fit (positive_double) - the standard error the growth rate

  - growth.0.025 (double) - lower confidence limit of the growth rate

  - growth.0.5 (double) - median estimate of the growth rate

  - growth.0.975 (double) - upper confidence limit of the growth rate

  Any grouping allowed.

  OR with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - relative.growth.fit (double) - an estimate of the relative growth
    rate

  - relative.growth.se.fit (positive_double) - the standard error the
    relative growth rate

  - relative.growth.0.025 (double) - lower confidence limit of the
    relative growth rate

  - relative.growth.0.5 (double) - median estimate of the relative
    growth rate

  - relative.growth.0.975 (double) - upper confidence limit of the
    relative growth rate

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
  are multiple incidence time series in the plot

- events:

  Significant events or time spans - a dataframe with columns:

  - label (character) - the event label

  - start (date) - the start date, or the date of the event

  - end (date) - the end date or NA if a single event

  Any grouping allowed.

  A default value is defined.

## Value

a ggplot

## Examples

``` r
data = example_poisson_rt_2class()
tmp2 = data %>% poisson_locfit_model()

if(interactive()) {
  plot_growth_rate(tmp2)
}
```
