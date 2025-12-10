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
#> $ incidence.fit       <dbl> 2.436120, 2.258592, 2.093156, 1.939683, 1.798045, …
#> $ incidence.se.fit    <dbl> 0.2692445, 0.2381464, 0.2131325, 0.1936194, 0.1790…
#> $ incidence.0.025     <dbl> 6.742364, 6.000443, 5.341058, 4.759755, 4.250883, …
#> $ incidence.0.05      <dbl> 7.339365, 6.468056, 5.712084, 5.059197, 4.497606, …
#> $ incidence.0.25      <dbl> 9.530694, 8.149563, 7.024472, 6.104883, 5.350995, …
#> $ incidence.0.5       <dbl> 11.428614, 9.569603, 8.110469, 6.956547, 6.037834,…
#> $ incidence.0.75      <dbl> 13.704480, 11.237080, 9.364364, 7.927023, 6.812835…
#> $ incidence.0.95      <dbl> 17.796253, 14.158394, 11.515886, 9.565462, 8.10552…
#> $ incidence.0.975     <dbl> 19.372018, 15.261756, 12.315858, 10.167235, 8.5759…
#> $ growth.fit          <dbl> -0.1836177419, -0.1722789078, -0.1606358336, -0.14…
#> $ growth.se.fit       <dbl> 0.042258642, 0.039649091, 0.037032501, 0.034429498…
#> $ growth.0.025        <dbl> -0.266443158, -0.249989698, -0.233218201, -0.21626…
#> $ growth.0.05         <dbl> -0.253127023, -0.237495859, -0.221548876, -0.20541…
#> $ growth.0.25         <dbl> -0.212120763, -0.199021813, -0.185613876, -0.17200…
#> $ growth.0.5          <dbl> -0.1836177419, -0.1722789078, -0.1606358336, -0.14…
#> $ growth.0.75         <dbl> -0.155114721, -0.145536003, -0.135657792, -0.12556…
#> $ growth.0.95         <dbl> -0.114108461, -0.107061957, -0.099722791, -0.09215…
#> $ growth.0.975        <dbl> -0.100792325, -0.094568118, -0.088053466, -0.08130…
#> $ doubling_time.0.5   <dbl> -3.774947, -4.023401, -4.315022, -4.658786, -5.066…
#> $ doubling_time.0.025 <dbl> -6.876984, -7.329607, -7.871890, -8.525562, -9.320…
#> $ doubling_time.0.975 <dbl> -2.601482, -2.772703, -2.972097, -3.205107, -3.478…

```
