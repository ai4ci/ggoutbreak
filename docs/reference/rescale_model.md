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
#> $ incidence.fit    <dbl> 2.618092, 2.630546, 2.643209, 2.656074, 2.669133, 2.6…
#> $ incidence.se.fit <dbl> 0.12540110, 0.11690289, 0.10926298, 0.10243521, 0.096…
#> $ incidence.0.025  <dbl> 10.72214, 11.03885, 11.34818, 11.64998, 11.94417, 12.…
#> $ incidence.0.05   <dbl> 78.08015, 80.17151, 82.21991, 84.22508, 86.18724, 88.…
#> $ incidence.0.25   <dbl> 88.18356, 89.80193, 91.41617, 93.02723, 94.63618, 96.…
#> $ incidence.0.5    <dbl> 13.70954, 13.88135, 14.05824, 14.24027, 14.42745, 14.…
#> $ incidence.0.75   <dbl> 104.4370, 105.1414, 105.9340, 106.8125, 107.7751, 108…
#> $ incidence.0.95   <dbl> 117.9509, 117.7713, 117.7826, 117.9752, 118.3402, 118…
#> $ incidence.0.975  <dbl> 17.52930, 17.45579, 17.41550, 17.40649, 17.42702, 17.…
#> $ growth.fit       <dbl> 0.01227444, 0.01242417, 0.01257663, 0.01273131, 0.012…
#> $ growth.se.fit    <dbl> 0.011313796, 0.010870712, 0.010427176, 0.009984404, 0…
#> $ growth.0.025     <dbl> -9.900189e-03, -8.882038e-03, -7.860260e-03, -6.83775…
#> $ growth.0.05      <dbl> -0.044345663, -0.038195945, -0.032021842, -0.02584098…
#> $ growth.0.25      <dbl> 0.03250383, 0.03564388, 0.03880524, 0.04197855, 0.045…
#> $ growth.0.5       <dbl> 0.01227444, 0.01242417, 0.01257663, 0.01273131, 0.012…
#> $ growth.0.75      <dbl> 0.1393384, 0.1382945, 0.1372676, 0.1362598, 0.1352734…
#> $ growth.0.95      <dbl> 0.2161879, 0.2121343, 0.2080947, 0.2040794, 0.2000988…
#> $ growth.0.975     <dbl> 0.03444907, 0.03373037, 0.03301352, 0.03230038, 0.031…
```
