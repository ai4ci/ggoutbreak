# Plot a proportions timeseries

Plot a proportions timeseries

## Usage

``` r
plot_proportion(
  modelled = i_proportion_model,
  raw = i_proportion_data,
  ...,
  mapping = .check_for_aes(modelled, ...),
  events = i_events
)
```

## Arguments

- modelled:

  Proportion model estimates - a dataframe with columns:

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

  Any grouping allowed.

- raw:

  Raw count data with denominator - a dataframe with columns:

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
tmp = example_poisson_rt_2class() %>%
  proportion_locfit_model(window=21) %>%
  dplyr::glimpse()
#> Rows: 322
#> Columns: 20
#> Groups: class [2]
#> $ class                  <fct> one, one, one, one, one, one, one, one, one, on…
#> $ time                   <t[day]> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13…
#> $ proportion.fit         <dbl> -0.33057858, -0.28970647, -0.24935786, -0.20927…
#> $ proportion.se.fit      <dbl> 0.3042480, 0.2900854, 0.2765568, 0.2637498, 0.2…
#> $ proportion.0.025       <dbl> 0.2835553, 0.2977023, 0.3118683, 0.3260266, 0.3…
#> $ proportion.0.05        <dbl> 0.3034290, 0.3171599, 0.3308699, 0.3445458, 0.3…
#> $ proportion.0.25        <dbl> 0.3691673, 0.3809858, 0.3927201, 0.4043992, 0.4…
#> $ proportion.0.5         <dbl> 0.4180999, 0.4280757, 0.4379816, 0.4478722, 0.4…
#> $ proportion.0.75        <dbl> 0.4686994, 0.4765056, 0.4842994, 0.4921569, 0.5…
#> $ proportion.0.95        <dbl> 0.5423644, 0.5467237, 0.5512042, 0.5559049, 0.5…
#> $ proportion.0.975       <dbl> 0.5660459, 0.5692644, 0.5726530, 0.5763161, 0.5…
#> $ relative.growth.fit    <dbl> 0.04122094, 0.04136694, 0.04176321, 0.04234719,…
#> $ relative.growth.se.fit <dbl> 0.02126269, 0.02121428, 0.02108289, 0.02088926,…
#> $ relative.growth.0.025  <dbl> -0.0004531581, -0.0002122864, 0.0004415081, 0.0…
#> $ relative.growth.0.05   <dbl> 0.006246934, 0.006472552, 0.007084944, 0.007987…
#> $ relative.growth.0.25   <dbl> 0.02687948, 0.02705812, 0.02754302, 0.02825760,…
#> $ relative.growth.0.5    <dbl> 0.04122094, 0.04136694, 0.04176321, 0.04234719,…
#> $ relative.growth.0.75   <dbl> 0.05556240, 0.05567575, 0.05598340, 0.05643677,…
#> $ relative.growth.0.95   <dbl> 0.07619495, 0.07626132, 0.07644147, 0.07670695,…
#> $ relative.growth.0.975  <dbl> 0.08289504, 0.08294616, 0.08308490, 0.08328938,…

if(interactive()) {
  plot_proportion(tmp)+
    ggplot2::scale_fill_viridis_d(aesthetics = c("fill","colour"))
}
```
