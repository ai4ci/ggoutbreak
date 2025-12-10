# Add a "true" time series to a plot

Add a "true" time series to a plot

## Usage

``` r
geom_truth(
  true_df = NULL,
  ...,
  true_col = NULL,
  true_fmt = list(colour = "red")
)
```

## Arguments

- true_df:

  a data frame with a `time_period` column called time and a value
  column (name given in `true_col`). This is optional and will be picked
  up from the `raw` parameter if given.

- ...:

  Named arguments passed on to
  [`as.time_period`](https://ai4ci.github.io/ggoutbreak/reference/as.time_period.md)

  `unit`

  :   the length of one unit of time. This will be either a integer
      number of days, or a specification such as "1 week", or another
      `time_period`. If `x` is a `time_period`, and the unit is
      different to that of `x` this will return a rescaled `time_period`
      using the new units.

  `start_date`

  :   the zero time date as something that can be coerced to a date. If
      the `x` input is already a `time_period` and this is different to
      its `start_date` then `x` will be recalibrated to use the new
      start date.

  `anchor`

  :   only relevant if `x` is a vector of dates, this is a date, or
      `"start"` or `"end"` or a weekday name e.g. `"mon"`. With the
      vector of dates in `x` it will use this anchor to find a reference
      date for the time-series. If not provided then the current
      defaults will be used. (see
      [`set_defaults()`](https://ai4ci.github.io/ggoutbreak/reference/set_defaults.md))

- true_col:

  the column name / expression of the true value

- true_fmt:

  a list of ggplot formatting to apply to the true value timeseries

## Value

A geom with the true value as a line.
