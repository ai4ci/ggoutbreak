# Plot a multinomial proportions model

A multinomial proportions model will tell you what proportion each class
has versus others in the data set. In this case the denominator is the
total count across across all classes.

## Usage

``` r
plot_multinomial(
  modelled = i_multinomial_proportion_model,
  ...,
  mapping = ggplot2::aes(fill = class),
  events = i_events,
  normalise = FALSE
)
```

## Arguments

- modelled:

  the multinomial count data - a dataframe with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - class (factor) - A factor specifying the type of observation. This
    will be things like variant, or serotype, for a multinomial model.
    Any missing data points are ignored.

  - proportion.0.5 (proportion) - median estimate of proportion (true
    scale)

  Must be grouped by: class (exactly).

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
  mapping. Usually this will be left as the default

- events:

  Significant events or time spans - a dataframe with columns:

  - label (character) - the event label

  - start (date) - the start date, or the date of the event

  - end (date) - the end date or NA if a single event

  Any grouping allowed.

  A default value is defined.

- normalise:

  make sure the probabilities add up to one - this can be a bad idea if
  you know you may have missing values, on the other hand not all
  proportions models are guaranteed to add up to one.

## Value

a ggplot

## Examples

``` r
tmp = example_proportion_age_stratified() %>%
  dplyr::group_by(class) %>%
  dplyr::glimpse()
#> Rows: 26,790
#> Columns: 20
#> Groups: class [19]
#> $ class                  <fct> 00_04, 00_04, 00_04, 00_04, 00_04, 00_04, 00_04…
#> $ time                   <t[day]> 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, …
#> $ proportion.fit         <dbl> -4.153966, -4.030468, -3.909366, -3.789972, -3.…
#> $ proportion.se.fit      <dbl> 2.786922, 2.612521, 2.449106, 2.296631, 2.15505…
#> $ proportion.0.025       <dbl> 6.663374e-05, 1.061114e-04, 1.649795e-04, 2.506…
#> $ proportion.0.05        <dbl> 0.0001603414, 0.0002416732, 0.0003568686, 0.000…
#> $ proportion.0.25        <dbl> 0.002390842, 0.003040809, 0.003829202, 0.004777…
#> $ proportion.0.5         <dbl> 0.01545928, 0.01745590, 0.01965899, 0.02209692,…
#> $ proportion.0.75        <dbl> 0.09328110, 0.09377845, 0.09470714, 0.09613571,…
#> $ proportion.0.95        <dbl> 0.6059008, 0.5662942, 0.5297285, 0.4969123, 0.4…
#> $ proportion.0.975       <dbl> 0.7872289, 0.7483779, 0.7090538, 0.6706974, 0.6…
#> $ relative.growth.fit    <dbl> 0.12492549, 0.12510534, 0.12559852, 0.12633546,…
#> $ relative.growth.se.fit <dbl> 0.2055049, 0.2043951, 0.2013519, 0.1968045, 0.1…
#> $ relative.growth.0.025  <dbl> -0.2778567, -0.2755017, -0.2690439, -0.2593943,…
#> $ relative.growth.0.05   <dbl> -0.2131000, -0.2110947, -0.2055958, -0.1973791,…
#> $ relative.growth.0.25   <dbl> -0.013685471, -0.012757065, -0.010211256, -0.00…
#> $ relative.growth.0.5    <dbl> 0.12492549, 0.12510534, 0.12559852, 0.12633546,…
#> $ relative.growth.0.75   <dbl> 0.26353645, 0.26296775, 0.26140830, 0.25907808,…
#> $ relative.growth.0.95   <dbl> 0.46295099, 0.46130538, 0.45679289, 0.45005005,…
#> $ relative.growth.0.975  <dbl> 0.5277077, 0.5257124, 0.5202409, 0.5120652, 0.5…

if(interactive()) {
  plot_multinomial(tmp, normalise=TRUE)+
    ggplot2::scale_fill_viridis_d()
}
```
