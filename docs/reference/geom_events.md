# Add time series event markers to a time series plot.

The x axis must be a date.

## Usage

``` r
geom_events(
  events = i_events,
  event_label_size = 7,
  event_label_colour = "black",
  event_label_angle = -30,
  event_line_colour = "grey50",
  event_fill_colour = "grey50",
  hide_labels = FALSE,
  guide_axis = ggplot2::derive(),
  x_axis_style = c("date", "time_period"),
  ...
)
```

## Arguments

- events:

  Significant events or time spans - a dataframe with columns:

  - label (character) - the event label

  - start (date) - the start date, or the date of the event

  - end (date) - the end date or NA if a single event

  Any grouping allowed.

  A default value is defined.

- event_label_size:

  how big to make the event label

- event_label_colour:

  the event label colour

- event_label_angle:

  the event label colour

- event_line_colour:

  the event line colour

- event_fill_colour:

  the event area fill

- hide_labels:

  do not show labels at all

- guide_axis:

  a guide axis configuration for the labels (see
  [`ggplot2::guide_axis`](https://ggplot2.tidyverse.org/reference/guide_axis.html)
  and
  [`ggplot2::dup_axis`](https://ggplot2.tidyverse.org/reference/sec_axis.html)).
  This can be used to specify a position amongst other things.

- x_axis_style:

  should the x-axis be `"date"` or a count of `"time_period"`s since a
  start date (may be specified in `...` or defaults from data)

- ...:

  Named arguments passed on to
  [`ggplot2::scale_x_date`](https://ggplot2.tidyverse.org/reference/scale_date.html)

  `name`

  :   The name of the scale. Used as the axis or legend title. If
      `waiver()`, the default, the name of the scale is taken from the
      first mapping used for that aesthetic. If `NULL`, the legend title
      will be omitted.

  `breaks`

  :   One of:

      - `NULL` for no breaks

      - `waiver()` for the breaks specified by `date_breaks`

      - A `Date`/`POSIXct` vector giving positions of breaks

      - A function that takes the limits as input and returns breaks as
        output

  `date_breaks`

  :   A string giving the distance between breaks like "2 weeks", or "10
      years". If both `breaks` and `date_breaks` are specified,
      `date_breaks` wins. Valid specifications are 'sec', 'min', 'hour',
      'day', 'week', 'month' or 'year', optionally followed by 's'.

  `labels`

  :   One of the options below. Please note that when `labels` is a
      vector, it is highly recommended to also set the `breaks` argument
      as a vector to protect against unintended mismatches.

      - `NULL` for no labels

      - `waiver()` for the default labels computed by the transformation
        object

      - A character vector giving labels (must be same length as
        `breaks`)

      - An expression vector (must be the same length as breaks). See
        ?plotmath for details.

      - A function that takes the breaks as input and returns labels as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `date_labels`

  :   A string giving the formatting specification for the labels. Codes
      are defined in
      [`strftime()`](https://rdrr.io/r/base/strptime.html). If both
      `labels` and `date_labels` are specified, `date_labels` wins.

  `minor_breaks`

  :   One of:

      - `NULL` for no breaks

      - `waiver()` for the breaks specified by `date_minor_breaks`

      - A `Date`/`POSIXct` vector giving positions of minor breaks

      - A function that takes the limits as input and returns minor
        breaks as output

  `date_minor_breaks`

  :   A string giving the distance between minor breaks like "2 weeks",
      or "10 years". If both `minor_breaks` and `date_minor_breaks` are
      specified, `date_minor_breaks` wins. Valid specifications are
      'sec', 'min', 'hour', 'day', 'week', 'month' or 'year', optionally
      followed by 's'.

  `limits`

  :   One of:

      - `NULL` to use the default scale range

      - A numeric vector of length two providing limits of the scale.
        Use `NA` to refer to the existing minimum or maximum

      - A function that accepts the existing (automatic) limits and
        returns new limits. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. Note that setting limits on positional scales
        will **remove** data outside of the limits. If the purpose is to
        zoom, use the limit argument in the coordinate system (see
        [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)).

  `expand`

  :   For position scales, a vector of range expansion constants used to
      add some padding around the data to ensure that they are placed
      some distance away from the axes. Use the convenience function
      [`expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
      to generate the values for the `expand` argument. The defaults are
      to expand the scale by 5% on each side for continuous variables,
      and by 0.6 units on each side for discrete variables.

  `oob`

  :   One of:

      - Function that handles limits outside of the scale limits (out of
        bounds). Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

      - The default
        ([`scales::censor()`](https://scales.r-lib.org/reference/oob.html))
        replaces out of bounds values with `NA`.

      - [`scales::squish()`](https://scales.r-lib.org/reference/oob.html)
        for squishing out of bounds values into range.

      - [`scales::squish_infinite()`](https://scales.r-lib.org/reference/oob.html)
        for squishing infinite values into range.

  `guide`

  :   A function used to create a guide or its name. See
      [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html)
      for more information.

  `position`

  :   For position scales, The position of the axis. `left` or `right`
      for y axes, `top` or `bottom` for x axes.

  `timezone`

  :   The timezone to use for display on the axes. The default (`NULL`)
      uses the timezone encoded in the data.

  `na.value`

  :   Missing values will be replaced with this value.

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

## Value

a set of `geoms` for a time series.
