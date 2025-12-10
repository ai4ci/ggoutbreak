# Plot a raw case count timeseries

Plot a raw case count timeseries

## Usage

``` r
plot_counts(raw, ..., mapping = .check_for_aes(raw, ...), events = i_events)
```

## Arguments

- raw:

  The raw count data, or the raw count data normalised by population (
  see
  [`normalise_count()`](https://ai4ci.github.io/ggoutbreak/reference/normalise_count.md)) -
  EITHER: a dataframe with columns:

  - population (positive_integer) - Size of population

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - population_unit (double) - The population unit on which the per
    capita incidence rate is calculated

  - time_unit (lubridate::as.period) - The time period over which the
    per capita incidence rate is calculated

  Any grouping allowed.

  OR with columns:

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

tmp = example_england_covid_by_age() %>%
  time_aggregate(count=sum(count)) %>%
  normalise_count(pop=56489700, population_unit=1000, normalise_time=TRUE)

# normalised by England population (56489700 people)

if(interactive()) {
  plot_counts(tmp, colour="blue",size=0.25)
}
```
