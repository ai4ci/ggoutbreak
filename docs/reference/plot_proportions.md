# Plot a raw case count proportion timeseries

Plot a raw case count proportion timeseries

## Usage

``` r
plot_proportions(
  raw = i_proportion_data,
  ...,
  mapping = .check_for_aes(raw, ...),
  events = i_events
)
```

## Arguments

- raw:

  The raw count and denominator data - a dataframe with columns:

  - denom (positive_integer) - Total test counts associated with the
    specified time frame

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

- ...:

  Named arguments passed on to
  [`geom_events`](https://ai4ci.github.io/ggoutbreak/reference/geom_events.md)

  `event_label_size`

  :   how big to make the event label

  `event_label_colour`

  :   the event label colour

  `event_label_angle`

  :   the event label colour

  `event_line_colour`

  :   the event line colour

  `event_fill_colour`

  :   the event area fill

  `hide_labels`

  :   do not show labels at all

  `guide_axis`

  :   a guide axis configuration for the labels (see
      [`ggplot2::guide_axis`](https://ggplot2.tidyverse.org/reference/guide_axis.html)
      and
      [`ggplot2::dup_axis`](https://ggplot2.tidyverse.org/reference/sec_axis.html)).
      This can be used to specify a position amongst other things.

  `...`

  :   Named arguments passed on to
      [`ggplot2::scale_x_date`](https://ggplot2.tidyverse.org/reference/scale_date.html)

      `name`

      :   The name of the scale. Used as the axis or legend title. If
          [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html),
          the default, the name of the scale is taken from the first
          mapping used for that aesthetic. If `NULL`, the legend title
          will be omitted.

      `breaks`

      :   One of:

          - `NULL` for no breaks

          - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
            for the breaks specified by `date_breaks`

          - A `Date`/`POSIXct` vector giving positions of breaks

          - A function that takes the limits as input and returns breaks
            as output

      `date_breaks`

      :   A string giving the distance between breaks like "2 weeks", or
          "10 years". If both `breaks` and `date_breaks` are specified,
          `date_breaks` wins. Valid specifications are 'sec', 'min',
          'hour', 'day', 'week', 'month' or 'year', optionally followed
          by 's'.

      `labels`

      :   One of:

          - `NULL` for no labels

          - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
            for the default labels computed by the transformation object

          - A character vector giving labels (must be same length as
            `breaks`)

          - An expression vector (must be the same length as breaks).
            See ?plotmath for details.

          - A function that takes the breaks as input and returns labels
            as output. Also accepts rlang
            [lambda](https://rlang.r-lib.org/reference/as_function.html)
            function notation.

      `date_labels`

      :   A string giving the formatting specification for the labels.
          Codes are defined in
          [`strftime()`](https://rdrr.io/r/base/strptime.html). If both
          `labels` and `date_labels` are specified, `date_labels` wins.

      `minor_breaks`

      :   One of:

          - `NULL` for no breaks

          - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
            for the breaks specified by `date_minor_breaks`

          - A `Date`/`POSIXct` vector giving positions of minor breaks

          - A function that takes the limits as input and returns minor
            breaks as output

      `date_minor_breaks`

      :   A string giving the distance between minor breaks like "2
          weeks", or "10 years". If both `minor_breaks` and
          `date_minor_breaks` are specified, `date_minor_breaks` wins.
          Valid specifications are 'sec', 'min', 'hour', 'day', 'week',
          'month' or 'year', optionally followed by 's'.

      `limits`

      :   One of:

          - `NULL` to use the default scale range

          - A numeric vector of length two providing limits of the
            scale. Use `NA` to refer to the existing minimum or maximum

          - A function that accepts the existing (automatic) limits and
            returns new limits. Also accepts rlang
            [lambda](https://rlang.r-lib.org/reference/as_function.html)
            function notation. Note that setting limits on positional
            scales will **remove** data outside of the limits. If the
            purpose is to zoom, use the limit argument in the coordinate
            system (see
            [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)).

      `expand`

      :   For position scales, a vector of range expansion constants
          used to add some padding around the data to ensure that they
          are placed some distance away from the axes. Use the
          convenience function
          [`expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
          to generate the values for the `expand` argument. The defaults
          are to expand the scale by 5% on each side for continuous
          variables, and by 0.6 units on each side for discrete
          variables.

      `oob`

      :   One of:

          - Function that handles limits outside of the scale limits
            (out of bounds). Also accepts rlang
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

      :   For position scales, The position of the axis. `left` or
          `right` for y axes, `top` or `bottom` for x axes.

      `sec.axis`

      :   [`sec_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html)
          is used to specify a secondary axis.

      `timezone`

      :   The timezone to use for display on the axes. The default
          (`NULL`) uses the timezone encoded in the data.

      `na.value`

      :   Missing values will be replaced with this value.

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

tmp = tibble::tibble(
  time = as.time_period(1:10, "1 day"),
  count = 101:110
) %>% dplyr::mutate(
  denom = count*time
)

if(interactive()) {
  plot_proportions(tmp)+ggplot2::geom_line()
}
```
