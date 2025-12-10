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
#> $ proportion.fit         <dbl> -0.125466401, -0.093250999, -0.061407396, -0.02…
#> $ proportion.se.fit      <dbl> 0.2924304, 0.2793603, 0.2668764, 0.2550528, 0.2…
#> $ proportion.0.025       <dbl> 0.3321184, 0.3450686, 0.3579034, 0.3705956, 0.3…
#> $ proportion.0.05        <dbl> 0.3528645, 0.3652248, 0.3774514, 0.3895289, 0.4…
#> $ proportion.0.25        <dbl> 0.4200160, 0.4300428, 0.4399382, 0.4497208, 0.4…
#> $ proportion.0.5         <dbl> 0.4686745, 0.4767041, 0.4846530, 0.4925585, 0.5…
#> $ proportion.0.75        <dbl> 0.5179360, 0.5237757, 0.5296148, 0.5355057, 0.5…
#> $ proportion.0.95        <dbl> 0.5879601, 0.5905539, 0.5932857, 0.5962240, 0.5…
#> $ proportion.0.975       <dbl> 0.6100891, 0.6116576, 0.6134081, 0.6154136, 0.6…
#> $ relative.growth.fit    <dbl> 0.03245704, 0.03254020, 0.03276592, 0.03309857,…
#> $ relative.growth.se.fit <dbl> 0.01991538, 0.01986567, 0.01973073, 0.01953188,…
#> $ relative.growth.0.025  <dbl> -6.576396e-03, -6.395798e-03, -5.905605e-03, -5…
#> $ relative.growth.0.05   <dbl> -0.0003008528, -0.0001359203, 0.0003117536, 0.0…
#> $ relative.growth.0.25   <dbl> 0.01902431, 0.01914101, 0.01945775, 0.01992452,…
#> $ relative.growth.0.5    <dbl> 0.03245704, 0.03254020, 0.03276592, 0.03309857,…
#> $ relative.growth.0.75   <dbl> 0.04588976, 0.04593939, 0.04607410, 0.04627263,…
#> $ relative.growth.0.95   <dbl> 0.06521493, 0.06521632, 0.06522009, 0.06522566,…
#> $ relative.growth.0.975  <dbl> 0.07149047, 0.07147619, 0.07143745, 0.07138036,…

if(interactive()) {
  plot_proportion(tmp)+
    ggplot2::scale_fill_viridis_d(aesthetics = c("fill","colour"))
}
```
