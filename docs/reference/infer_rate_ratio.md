# Calculate a risk ratio from incidence

**\[experimental\]**

This enables incidence rates are able to be compared to a baseline
figure for incidence. The baseline could come for example from a
population average or average incidence over time. The output is an
incidence rate ratio. The `incidence_baseline` column is a rate of
events per unit time. The time unit is expected to be the same as that
of the date in `modelled` and this is not checked.

## Usage

``` r
infer_rate_ratio(
  modelled = i_incidence_model,
  base = i_baseline_incidence_data,
  ...
)
```

## Arguments

- modelled:

  Model output from something like
  [`poisson_locfit_model()`](https://ai4ci.github.io/ggoutbreak/reference/poisson_locfit_model.md).
  It really makes sense if this is a grouped model. - a dataframe with
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

  Any grouping allowed.

- base:

  The baseline data must be grouped in the same way as `modelled`. It
  may be a time series but does not have to be. See the example and note
  this may change in the future. - a dataframe with columns:

  - baseline_incidence (positive_double) - Baseline raw incidence rate
    as count data

  Any grouping allowed.

- ...:

  not used

## Value

a dataframe with incidence rate ratios for each of the classes in
modelled. A dataframe containing the following columns:

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

- rate_ratio.0.025 (positive_double) - lower confidence limit of the
  rate ratio for a population group

- rate_ratio.0.5 (positive_double) - median estimate of the rate ratio
  for a population group

- rate_ratio.0.975 (positive_double) - upper confidence limit of the
  rate ratio for a population group

Any grouping allowed.

## Examples

``` r
# not age stratified
baseline = example_poisson_locfit() %>%
  dplyr::mutate(baseline_incidence = incidence.0.5)

# age stratified rate ratios:
tmp = example_poisson_age_stratified() %>%
  infer_rate_ratio(baseline) %>%
  dplyr::glimpse()
#> Rows: 26,790
#> Columns: 24
#> Groups: class [19]
#> $ class              <fct> 00_04, 00_04, 00_04, 00_04, 00_04, 00_04, 00_04, 00…
#> $ time               <t[day]> 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, …
#> $ incidence.fit      <dbl> -1.612905, -1.803708, -1.959326, -2.082483, -2.1759…
#> $ incidence.se.fit   <dbl> 1.0533915, 0.9003108, 0.7844360, 0.7012956, 0.64635…
#> $ incidence.0.025    <dbl> 0.02528574, 0.02820419, 0.03029440, 0.03152429, 0.0…
#> $ incidence.0.05     <dbl> 0.03523977, 0.03745604, 0.03878940, 0.03932042, 0.0…
#> $ incidence.0.25     <dbl> 0.09793934, 0.08972926, 0.08304107, 0.07765344, 0.0…
#> $ incidence.0.5      <dbl> 0.19930774, 0.16468709, 0.14095339, 0.12462043, 0.1…
#> $ incidence.0.75     <dbl> 0.40559365, 0.30226304, 0.23925342, 0.19999437, 0.1…
#> $ incidence.0.95     <dbl> 1.1272371, 0.7240980, 0.5121982, 0.3949666, 0.32865…
#> $ incidence.0.975    <dbl> 1.5709871, 0.9616244, 0.6558260, 0.4926439, 0.40289…
#> $ growth.fit         <dbl> -0.2093030305, -0.1855586041, -0.1593989278, -0.131…
#> $ growth.se.fit      <dbl> 0.22418245, 0.20710820, 0.19030250, 0.17387303, 0.1…
#> $ growth.0.025       <dbl> -0.64869255, -0.59148321, -0.53238498, -0.47246827,…
#> $ growth.0.05        <dbl> -0.57805034, -0.52622127, -0.47241869, -0.41767908,…
#> $ growth.0.25        <dbl> -0.360511792, -0.325250959, -0.287756014, -0.248958…
#> $ growth.0.5         <dbl> -0.2093030305, -0.1855586041, -0.1593989278, -0.131…
#> $ growth.0.75        <dbl> -0.058094269, -0.045866249, -0.031041841, -0.014407…
#> $ growth.0.95        <dbl> 0.15944428, 0.15510406, 0.15362083, 0.15431230, 0.1…
#> $ growth.0.975       <dbl> 0.23008649, 0.22036600, 0.21358712, 0.20910150, 0.2…
#> $ baseline_incidence <dbl> 6.530236, 5.968283, 5.379160, 4.794581, 4.238230, 3…
#> $ rate_ratio.0.025   <dbl> 0.003872102, 0.004725680, 0.005631810, 0.006574984,…
#> $ rate_ratio.0.5     <dbl> 0.03052075, 0.02759372, 0.02620361, 0.02599193, 0.0…
#> $ rate_ratio.0.975   <dbl> 0.24057125, 0.16112246, 0.12191979, 0.10275015, 0.0…

```
