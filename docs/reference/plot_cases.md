# Plot a line-list of cases as a histogram

Plot a line-list of cases as a histogram

## Usage

``` r
plot_cases(
  raw,
  ...,
  mapping = .check_for_aes(raw, ..., class_aes = "fill"),
  events = i_events
)
```

## Arguments

- raw:

  The raw case data either as a summarised count or as a line-list -
  EITHER: a dataframe with columns:

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

  OR with columns:

  - time (ggoutbreak::time_period) - A set of events with a timestamp as
    a \`time_period\`

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

- mapping:

  a [`ggplot2::aes`](https://ggplot2.tidyverse.org/reference/aes.html)
  mapping. Most importantly setting the `fill` to something if there are
  multiple types of event in the plot. If a `class` column is present
  the mapping will default to using this.

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
with_defaults("2025-01-01" ,"1 day", {
  tmp = dplyr::tibble(
    time = as.time_period( rexpgrowth(100,0.05,40)),
    class = rep(c("one","two","three"), length.out=100)
  )
})

if(interactive()) {
  plot_cases(tmp, mapping=ggplot2::aes(fill = class),linewidth=0.1,colour="white")
}
```
