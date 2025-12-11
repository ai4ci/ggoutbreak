# Rescale a timeseries in the temporal dimension

Sometimes we may have, for example, modelled incidence or growth rates
on weekly data resulting in cases per week and growth rate per week. We
may wish to use this to estimate the reproduction number, using
algorithms that assume a daily incidence. Not everything has a
dependence on time, and things such as proportions, or prevalence will
not change.

## Usage

``` r
rescale_model(df = i_timeseries, time_unit)
```

## Arguments

- df:

  A data frame containing modelled output. This will modify the
  following columns if present:

  A dataframe containing the following columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a `time_period`

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

  Any grouping allowed.

  A dataframe containing the following columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a `time_period`

  - growth.fit (double) - an estimate of the growth rate

  - growth.se.fit (positive_double) - the standard error the growth rate

  - growth.0.025 (double) - lower confidence limit of the growth rate

  - growth.0.5 (double) - median estimate of the growth rate

  - growth.0.975 (double) - upper confidence limit of the growth rate

  Any grouping allowed.

  A dataframe containing the following columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a `time_period`

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

- time_unit:

  a `lubridate` period string such as "1 day"

## Value

the same time series with different time unit, and adjusted incidence
and growth rate figures.

## Examples

``` r
sim = sim_poisson_model(time_unit = "1 week")
incidence = sim %>% poisson_locfit_model(frequency = "1 day", deg = 2, window=5)
incidence2 = incidence %>% rescale_model(time_unit = "1 day")
incidence2 %>% dplyr::glimpse()
#> Rows: 727
#> Columns: 20
#> Groups: statistic [1]
#> $ statistic        <chr> "infections", "infections", "infections", "infections…
#> $ time             <t[day]> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,…
#> $ incidence.fit    <dbl> 2.602330, 2.619494, 2.636412, 2.653095, 2.669555, 2.6…
#> $ incidence.se.fit <dbl> 0.08919687, 0.08304502, 0.07751475, 0.07257184, 0.068…
#> $ incidence.0.025  <dbl> 11.33061, 11.66659, 11.99495, 12.31548, 12.62804, 12.…
#> $ incidence.0.05   <dbl> 81.57514, 83.83139, 86.04082, 88.20250, 90.31602, 92.…
#> $ incidence.0.25   <dbl> 88.95032, 90.86650, 92.76221, 94.63774, 96.49360, 98.…
#> $ incidence.0.5    <dbl> 13.49515, 13.72878, 13.96302, 14.19792, 14.43354, 14.…
#> $ incidence.0.75   <dbl> 100.3238, 101.6380, 102.9872, 104.3713, 105.7897, 107…
#> $ incidence.0.95   <dbl> 109.3940, 110.1674, 111.0325, 111.9862, 113.0257, 114…
#> $ incidence.0.975  <dbl> 16.07319, 16.15548, 16.25399, 16.36808, 16.49719, 16.…
#> $ growth.fit       <dbl> 0.01742825, 0.01727977, 0.01711183, 0.01692751, 0.016…
#> $ growth.se.fit    <dbl> 0.008068884, 0.007744598, 0.007420038, 0.007096142, 0…
#> $ growth.0.025     <dbl> 0.001613532, 0.002100635, 0.002568820, 0.003019323, 0…
#> $ growth.0.05      <dbl> 0.02909285, 0.03178727, 0.03434865, 0.03678774, 0.039…
#> $ growth.0.25      <dbl> 0.08390112, 0.08439282, 0.08474961, 0.08498862, 0.085…
#> $ growth.0.5       <dbl> 0.01742825, 0.01727977, 0.01711183, 0.01692751, 0.016…
#> $ growth.0.75      <dbl> 0.1600944, 0.1575239, 0.1548160, 0.1519965, 0.1490914…
#> $ growth.0.95      <dbl> 0.2149027, 0.2101295, 0.2052169, 0.2001974, 0.1951031…
#> $ growth.0.975     <dbl> 0.03324297, 0.03245890, 0.03165483, 0.03083569, 0.030…
```
