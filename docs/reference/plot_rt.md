# Reproduction number timeseries diagram

Reproduction number timeseries diagram

## Usage

``` r
plot_rt(
  modelled = i_reproduction_number,
  ...,
  mapping = .check_for_aes(modelled, ...),
  events = i_events
)
```

## Arguments

- modelled:

  the modelled Rt estimate - a dataframe with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - rt.fit (double) - an estimate of the reproduction number

  - rt.se.fit (positive_double) - the standard error of the reproduction
    number

  - rt.0.025 (double) - lower confidence limit of the reproduction
    number

  - rt.0.5 (double) - median estimate of the reproduction number

  - rt.0.975 (double) - upper confidence limit of the reproduction
    number

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

a ggplot timeseries

## Examples

``` r
# example code
if (interactive()) {

tmp2 = example_poisson_locfit() %>%
  dplyr::filter(as.Date(time) >= "2021-01-01" & as.Date(time) < "2022-01-01") %>%
  rt_from_incidence(ip = example_ganyani_ip())

# comparing RT from growth rates with England consensus Rt
# (N.B. offset by 14 days to align with estimates):

plot_rt(tmp2,colour="blue")+
  ggplot2::geom_errorbar(
    data= ukc19::spim_consensus %>%
      dplyr::filter(date-14 >= "2021-01-01" & date-14 < "2022-01-01"),
    mapping=ggplot2::aes(x=date-14,ymin=rt.low,ymax=rt.high),
    colour="red")

}
```
