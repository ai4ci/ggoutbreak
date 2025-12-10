# Plot an incidence or proportion versus growth phase diagram

Plot an incidence or proportion versus growth phase diagram

## Usage

``` r
plot_growth_phase(
  modelled,
  timepoints = NULL,
  duration = max(dplyr::count(modelled)$n),
  interval = 7,
  mapping = if (interfacer::is_col_present(modelled, class)) {
     ggplot2::aes(colour =
    class)
 } else {
     ggplot2::aes()
 },
  cis = TRUE,
  ...
)
```

## Arguments

- modelled:

  Growth rates and incidence / proportion timeseries as the outputs of
  functions such as `proportion_locfit_model`, `poisson_locfit_model`,
  or similar. - EITHER: a dataframe with columns:

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

  - growth.fit (double) - an estimate of the growth rate

  - growth.se.fit (positive_double) - the standard error the growth rate

  - growth.0.025 (double) - lower confidence limit of the growth rate

  - growth.0.5 (double) - median estimate of the growth rate

  - growth.0.975 (double) - upper confidence limit of the growth rate

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

  - growth.fit (double) - an estimate of the growth rate

  - growth.se.fit (positive_double) - the standard error the growth rate

  - growth.0.025 (double) - lower confidence limit of the growth rate

  - growth.0.5 (double) - median estimate of the growth rate

  - growth.0.975 (double) - upper confidence limit of the growth rate

  Any grouping allowed.

  OR with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - risk_ratio.0.025 (positive_double) - lower confidence limit of the
    excess risk ratio for a population group

  - risk_ratio.0.5 (positive_double) - median estimate of the excess
    risk ratio for a population group

  - risk_ratio.0.975 (positive_double) - upper confidence limit of the
    excess risk ratio for a population group

  - proportion.fit (double) - an estimate of the proportion on a logit
    scale

  - proportion.se.fit (positive_double) - the standard error of
    proportion estimate on a logit scale

  - proportion.0.025 (proportion) - lower confidence limit of proportion
    (true scale)

  - proportion.0.5 (proportion) - median estimate of proportion (true
    scale)

  - proportion.0.975 (proportion) - upper confidence limit of proportion
    (true scale)

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

  OR with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - proportion.fit (double) - an estimate of the proportion on a logit
    scale

  - proportion.se.fit (positive_double) - the standard error of
    proportion estimate on a logit scale

  - proportion.0.025 (proportion) - lower confidence limit of proportion
    (true scale)

  - proportion.0.5 (proportion) - median estimate of proportion (true
    scale)

  - proportion.0.975 (proportion) - upper confidence limit of proportion
    (true scale)

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

  OR with NULL

- timepoints:

  time points (as `Date` or `time_period` vector) of dates to plot phase
  diagrams. If multiple this will result in a sequence of plots as
  facets. If `NULL` (the default) it will be the last time point in the
  series

- duration:

  the length of the growth rate phase trail

- interval:

  the length of time between markers on the phase plot

- mapping:

  a [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
  mapping

- cis:

  logical; should the phases be marked with confidence intervals?

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
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

  `facets`

  :   A set of variables or expressions quoted by
      [`vars()`](https://ggplot2.tidyverse.org/reference/vars.html) and
      defining faceting groups on the rows or columns dimension. The
      variables can be named (the names are passed to `labeller`).

      For compatibility with the classic interface, can also be a
      formula or character vector. Use either a one sided formula,
      `~a + b`, or a character vector, `c("a", "b")`.

  `nrow,ncol`

  :   Number of rows and columns.

  `scales`

  :   Should scales be fixed (`"fixed"`, the default), free (`"free"`),
      or free in one dimension (`"free_x"`, `"free_y"`)?

  `space`

  :   If `"fixed"` (default), all panels have the same size and the
      number of rows and columns in the layout can be arbitrary. If
      `"free_x"`, panels have widths proportional to the length of the
      x-scale, but the layout is constrained to one row. If `"free_y"`,
      panels have heights proportional to the length of the y-scale, but
      the layout is constrained to one column.

  `shrink`

  :   If `TRUE`, will shrink scales to fit output of statistics, not raw
      data. If `FALSE`, will be range of raw data before statistical
      summary.

  `labeller`

  :   A function that takes one data frame of labels and returns a list
      or data frame of character vectors. Each input column corresponds
      to one factor. Thus there will be more than one with
      `vars(cyl, am)`. Each output column gets displayed as one separate
      line in the strip label. This function should inherit from the
      "labeller" S3 class for compatibility with
      [`labeller()`](https://ggplot2.tidyverse.org/reference/labeller.html).
      You can use different labeling functions for different kind of
      labels, for example use
      [`label_parsed()`](https://ggplot2.tidyverse.org/reference/labellers.html)
      for formatting facet labels.
      [`label_value()`](https://ggplot2.tidyverse.org/reference/labellers.html)
      is used by default, check it for more details and pointers to
      other options.

  `as.table`

  :   **\[superseded\]** The `as.table` argument is now absorbed into
      the `dir` argument via the two letter options. If `TRUE`, the
      facets are laid out like a table with highest values at the
      bottom-right. If `FALSE`, the facets are laid out like a plot with
      the highest value at the top-right.

  `switch`

  :   By default, the labels are displayed on the top and right of the
      plot. If `"x"`, the top labels will be displayed to the bottom. If
      `"y"`, the right-hand side labels will be displayed to the left.
      Can also be set to `"both"`.

  `drop`

  :   If `TRUE`, the default, all factor levels not used in the data
      will automatically be dropped. If `FALSE`, all factor levels will
      be shown, regardless of whether or not they appear in the data.

  `dir`

  :   Direction: either `"h"` for horizontal, the default, or `"v"`, for
      vertical. When `"h"` or `"v"` will be combined with `as.table` to
      set final layout. Alternatively, a combination of `"t"` (top) or
      `"b"` (bottom) with `"l"` (left) or `"r"` (right) to set a layout
      directly. These two letters give the starting position and the
      first letter gives the growing direction. For example `"rt"` will
      place the first panel in the top-right and starts filling in
      panels right-to-left.

  `strip.position`

  :   By default, the labels are displayed on the top of the plot. Using
      `strip.position` it is possible to place the labels on either of
      the four sides by setting
      `strip.position = c("top", "bottom", "left", "right")`

  `axes`

  :   Determines which axes will be drawn in case of fixed scales. When
      `"margins"` (default), axes will be drawn at the exterior margins.
      `"all_x"` and `"all_y"` will draw the respective axes at the
      interior panels too, whereas `"all"` will draw all axes at all
      panels.

  `axis.labels`

  :   Determines whether to draw labels for interior axes when the scale
      is fixed and the `axis` argument is not `"margins"`. When `"all"`
      (default), all interior axes get labels. When `"margins"`, only
      the exterior axes get labels, and the interior axes get none. When
      `"all_x"` or `"all_y"`, only draws the labels at the interior axes
      in the x- or y-direction respectively.

## Value

a ggplot

## Examples

``` r
data = example_poisson_rt_2class()
tmp2 = data %>% poisson_locfit_model()

timepoints = as.Date(tmp2$time[c(40,80,120,160)])

if(interactive()) {
  plot_growth_phase(tmp2, timepoints, duration=108)
}
```
