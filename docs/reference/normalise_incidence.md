# Calculate a normalised incidence rate per capita

This assumes positive disease counts are stratified by a population
grouping, e.g. geography or age, and we have estimates of the size of
that population during that time period. Normalising by population size
allows us to compare groups.

## Usage

``` r
normalise_incidence(
  modelled = i_incidence_model,
  pop = i_population_data,
  ...,
  population_unit = 1e+05,
  normalise_time = FALSE
)
```

## Arguments

- modelled:

  Model output from processing the `raw` dataframe with something like
  `poission_locfit_model` - a dataframe with columns:

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

- pop:

  The population data must be grouped in the same way as `modelled`. - a
  dataframe with columns:

  - population (positive_integer) - Size of population

  Any grouping allowed.

- ...:

  not used

- population_unit:

  what population unit do you want the incidence in e.g. per 100K

- normalise_time:

  The default behaviour for incidence is to keep it in the same time
  units as the input data. If this parameter is set to `TRUE` the
  incidence rates are calculated per year. If given as a lubridate
  period string e.g. "1 day" then the incidence is calculated over that
  time period.

## Value

a dataframe with incidence rates per unit capita. A dataframe containing
the following columns:

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

- incidence.per_capita.fit (double) - an estimate of the incidence per
  capita rate on a log scale

- incidence.per_capita.se.fit (positive_double) - the standard error of
  the incidence per capita rate estimate on a log scale

- incidence.per_capita.0.025 (positive_double) - lower confidence limit
  of the incidence per capita rate (true scale)

- incidence.per_capita.0.5 (positive_double) - median estimate of the
  incidence per capita rate (true scale)

- incidence.per_capita.0.975 (positive_double) - upper confidence limit
  of the incidence per capita rate (true scale)

- population_unit (double) - The population unit on which the per capita
  incidence rate is calculated

- time_unit (lubridate::as.period) - The time period over which the per
  capita incidence rate is calculated

Any grouping allowed.

## Examples

``` r
model = example_poisson_age_stratified()
demog = ukc19::uk_population_2019_by_5yr_age

model %>%
  normalise_incidence(demog) %>%
  dplyr::glimpse()
#> Adding missing grouping variables: `name`, `code`, `codeType`
#> different column groupings in `modelled` and `base` parameters.
#> regrouping `base` data to be compatible with `modelled` grouping
#> Rows: 10,662,420
#> Columns: 31
#> Groups: class [19]
#> $ class                       <chr> "00_04", "00_04", "00_04", "00_04", "00_04…
#> $ time                        <t[day]> 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,…
#> $ incidence.fit               <dbl> -1.612905, -1.612905, -1.612905, -1.612905…
#> $ incidence.se.fit            <dbl> 1.053391, 1.053391, 1.053391, 1.053391, 1.…
#> $ incidence.0.025             <dbl> 0.02528574, 0.02528574, 0.02528574, 0.0252…
#> $ incidence.0.05              <dbl> 0.03523977, 0.03523977, 0.03523977, 0.0352…
#> $ incidence.0.25              <dbl> 0.09793934, 0.09793934, 0.09793934, 0.0979…
#> $ incidence.0.5               <dbl> 0.1993077, 0.1993077, 0.1993077, 0.1993077…
#> $ incidence.0.75              <dbl> 0.4055937, 0.4055937, 0.4055937, 0.4055937…
#> $ incidence.0.95              <dbl> 1.127237, 1.127237, 1.127237, 1.127237, 1.…
#> $ incidence.0.975             <dbl> 1.570987, 1.570987, 1.570987, 1.570987, 1.…
#> $ growth.fit                  <dbl> -0.209303, -0.209303, -0.209303, -0.209303…
#> $ growth.se.fit               <dbl> 0.2241824, 0.2241824, 0.2241824, 0.2241824…
#> $ growth.0.025                <dbl> -0.6486925, -0.6486925, -0.6486925, -0.648…
#> $ growth.0.05                 <dbl> -0.5780503, -0.5780503, -0.5780503, -0.578…
#> $ growth.0.25                 <dbl> -0.3605118, -0.3605118, -0.3605118, -0.360…
#> $ growth.0.5                  <dbl> -0.209303, -0.209303, -0.209303, -0.209303…
#> $ growth.0.75                 <dbl> -0.05809427, -0.05809427, -0.05809427, -0.…
#> $ growth.0.95                 <dbl> 0.1594443, 0.1594443, 0.1594443, 0.1594443…
#> $ growth.0.975                <dbl> 0.2300865, 0.2300865, 0.2300865, 0.2300865…
#> $ name                        <chr> "Hartlepool", "Middlesbrough", "Redcar and…
#> $ code                        <chr> "E06000001", "E06000002", "E06000003", "E0…
#> $ codeType                    <chr> "LAD19", "LAD19", "LAD19", "LAD19", "LAD19…
#> $ population                  <int> 5229, 9583, 7106, 11462, 5790, 7580, 11596…
#> $ incidence.per_capita.0.025  <dbl> 0.48356745, 0.26386040, 0.35583651, 0.2206…
#> $ incidence.per_capita.0.5    <dbl> 3.8115842, 2.0798052, 2.8047810, 1.7388565…
#> $ incidence.per_capita.0.975  <dbl> 30.043738, 16.393479, 22.107896, 13.706047…
#> $ incidence.per_capita.fit    <dbl> 1.33804490, 0.73227426, 1.03132546, 0.5532…
#> $ incidence.per_capita.se.fit <dbl> 1.053391, 1.053391, 1.053391, 1.053391, 1.…
#> $ population_unit             <dbl> 1e+05, 1e+05, 1e+05, 1e+05, 1e+05, 1e+05, …
#> $ time_unit                   <Period> 1d 0H 0M 0S, 1d 0H 0M 0S, 1d 0H 0M 0S, …
```
