# Plot a timeseries of disease prevalence

**\[experimental\]**

## Usage

``` r
plot_prevalence(
  modelled = i_prevalence_model,
  raw = i_proportion_data,
  ...,
  mapping = .check_for_aes(modelled, ...),
  events = i_events
)
```

## Arguments

- modelled:

  Prevalence estimates - a dataframe with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - prevalence.0.025 (proportion) - lower confidence limit of prevalence
    (true scale)

  - prevalence.0.5 (proportion) - median estimate of prevalence (true
    scale)

  - prevalence.0.975 (proportion) - upper confidence limit of prevalence
    (true scale)

  Any grouping allowed.

- raw:

  Raw proportion data - a dataframe with columns:

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
if(interactive()) {

  plot_prevalence(
    ukc19::ons_infection_survey %>%
      dplyr::mutate(time = as.time_period(date,"1 day")),
    mapping = ggplot2::aes(colour=name))
}
```
