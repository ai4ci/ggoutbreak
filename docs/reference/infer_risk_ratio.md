# Calculate a normalised risk ratio from proportions

**\[experimental\]**

This assumes that for example, case distribution proportions are
stratified by a population grouping, e.g. geography or age, and we have
estimates of the size of that population during that time period.
Normalising by population proportion allows us to compare the relative
risk of the outcome in groups, compared to the expected population risk
if the outcome is evenly distributed across the population. There may be
other proportions other than population fractions that may be useful to
compare. At the moment this does not handle any uncertainty.

## Usage

``` r
infer_risk_ratio(
  modelled = i_proportion_model,
  base = i_baseline_proportion_data,
  ...
)
```

## Arguments

- modelled:

  Model output from something like
  [`proportion_locfit_model()`](https://ai4ci.github.io/ggoutbreak/reference/proportion_locfit_model.md) -
  a dataframe with columns:

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

- base:

  The baseline data must be grouped in the same way as `modelled`. It
  may be a time series but does not have to be. - a dataframe with
  columns:

  - baseline_proportion (proportion) - Baseline proportion for
    comparison

  Any grouping allowed.

- ...:

  not used

## Value

a dataframe with relative risk / risk ratio columns. A dataframe
containing the following columns:

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

- risk_ratio.0.025 (positive_double) - lower confidence limit of the
  excess risk ratio for a population group

- risk_ratio.0.5 (positive_double) - median estimate of the excess risk
  ratio for a population group

- risk_ratio.0.975 (positive_double) - upper confidence limit of the
  excess risk ratio for a population group

Any grouping allowed.

## Examples

``` r
demog = ukc19::uk_population_2019_by_5yr_age %>%
  dplyr::filter(name == "England")


tmp = example_proportion_age_stratified() %>%
  infer_risk_ratio(demog) %>%
  dplyr::glimpse()
#> Adding missing grouping variables: `name`, `code`, `codeType`
#> different column groupings in `modelled` and `base` parameters.
#> regrouping `base` data to be compatible with `modelled` grouping
#> Rows: 26,790
#> Columns: 27
#> Groups: class [19]
#> $ class                  <chr> "00_04", "00_04", "00_04", "00_04", "00_04", "0…
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
#> $ name                   <chr> "England", "England", "England", "England", "En…
#> $ code                   <chr> "E92000001", "E92000001", "E92000001", "E920000…
#> $ codeType               <chr> "CTRY20", "CTRY20", "CTRY20", "CTRY20", "CTRY20…
#> $ baseline_proportion    <dbl> 0.05862169, 0.05862169, 0.05862169, 0.05862169,…
#> $ risk_ratio.0.025       <dbl> 0.001136674, 0.001810105, 0.002814308, 0.004275…
#> $ risk_ratio.0.5         <dbl> 0.2637127, 0.2977720, 0.3353535, 0.3769410, 0.4…
#> $ risk_ratio.0.975       <dbl> 13.428968, 12.766228, 12.095416, 11.441113, 10.…

if(interactive()) {
  plot_growth_phase(tmp,duration = 14*4)+
  ggplot2::scale_colour_viridis_d()+
  ggplot2::coord_cartesian(xlim=c(-0.25,0.25),ylim=c(0.02,20))
}
```
