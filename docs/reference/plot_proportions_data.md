# Plot a raw case count proportion timeseries

Plot a raw case count proportion timeseries

## Usage

``` r
plot_proportions_data(
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

  `events`

  :   Significant events or time spans - a dataframe with columns:

      - label (character) - the event label

      - start (date) - the start date, or the date of the event

      - end (date) - the end date or NA if a single event

      Any grouping allowed.

      A default value is defined.

- mapping:

  a [`ggplot2::aes`](https://ggplot2.tidyverse.org/reference/aes.html)
  mapping. Most importantly setting the `colour` to something if there
  are multiple count timeseries in the data

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
tmp = example_england_covid_by_age() %>%
  dplyr::filter(class %in% c("50_54","80_84"))

if(interactive()) {
  plot_proportions_data(tmp, mapping= ggplot2::aes(colour=class))+ggplot2::geom_line()
}
```
