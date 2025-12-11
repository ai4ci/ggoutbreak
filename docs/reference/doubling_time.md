# Doubling time from growth rate

The unit of doubling times is always days.

## Usage

``` r
doubling_time(x, ...)
```

## Arguments

- x:

  proportion or incidence growth rates - EITHER: a dataframe with
  columns:

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

- ...:

  not used

## Value

the same dataframe with additional columns for doubling time or relative
doubling time plus confidence intervals.

## Examples

``` r
example_poisson_rt_smooth() %>%
  poisson_locfit_model(window=21) %>%
  doubling_time() %>%
  dplyr::glimpse()
#> Rows: 161
#> Columns: 23
#> Groups: statistic [1]
#> $ statistic           <chr> "infections", "infections", "infections", "infecti…
#> $ time                <t[day]> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1…
#> $ incidence.fit       <dbl> 2.5234144, 2.3339883, 2.1567327, 1.9915858, 1.8384…
#> $ incidence.se.fit    <dbl> 0.3218402, 0.2840632, 0.2540868, 0.2311115, 0.2143…
#> $ incidence.0.025     <dbl> 6.636728, 5.913481, 5.252644, 4.658137, 4.130242, …
#> $ incidence.0.05      <dbl> 7.345106, 6.467218, 5.690494, 5.010026, 4.418875, …
#> $ incidence.0.25      <dbl> 10.037584, 8.519781, 7.281623, 6.269543, 5.440648,…
#> $ incidence.0.5       <dbl> 12.471105, 10.319015, 8.642853, 7.327144, 6.287011…
#> $ incidence.0.75      <dbl> 15.494611, 12.498217, 10.258552, 8.563150, 7.26503…
#> $ incidence.0.95      <dbl> 21.174432, 16.464896, 13.126963, 10.715919, 8.9449…
#> $ incidence.0.975     <dbl> 23.434507, 18.006663, 14.221199, 11.525431, 9.5700…
#> $ growth.fit          <dbl> -0.195531855, -0.183763734, -0.171804108, -0.15971…
#> $ growth.se.fit       <dbl> 0.051884572, 0.048622636, 0.045353832, 0.042104737…
#> $ growth.0.025        <dbl> -0.297223748, -0.279062350, -0.260695985, -0.24223…
#> $ growth.0.05         <dbl> -0.280874382, -0.263740853, -0.246404523, -0.22896…
#> $ growth.0.25         <dbl> -0.230527467, -0.216559203, -0.202394803, -0.18811…
#> $ growth.0.5          <dbl> -0.195531855, -0.183763734, -0.171804108, -0.15971…
#> $ growth.0.75         <dbl> -0.1605362428, -0.1509682637, -0.1412134134, -0.13…
#> $ growth.0.95         <dbl> -0.110189328, -0.103786614, -0.097203693, -0.09045…
#> $ growth.0.975        <dbl> -0.093839962, -0.088465117, -0.082912231, -0.07718…
#> $ doubling_time.0.5   <dbl> -3.544932, -3.771948, -4.034520, -4.339986, -4.697…
#> $ doubling_time.0.025 <dbl> -7.386482, -7.835260, -8.360011, -8.979977, -9.721…
#> $ doubling_time.0.975 <dbl> -2.332072, -2.483843, -2.658833, -2.861459, -3.097…

```
