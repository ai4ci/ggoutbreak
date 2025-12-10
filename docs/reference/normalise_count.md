# Calculate a normalised count per capita

This assumes positive disease counts are stratified by a population
grouping, e.g. geography or age, and we have estimates of the size of
that population during that time period. Normalising by population size
allows us to compare groups.

## Usage

``` r
normalise_count(
  raw = i_incidence_data,
  pop = i_population_data,
  ...,
  population_unit = 1e+05,
  normalise_time = FALSE
)
```

## Arguments

- raw:

  The count data - a dataframe with columns:

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

- pop:

  The population data must be grouped in the same way as `raw`. - a
  dataframe with columns:

  - population (positive_integer) - Size of population

  Any grouping allowed.

- ...:

  not used

- population_unit:

  What population unit do you want the count data normalised to e.g. per
  100K

- normalise_time:

  The default behaviour for normalising is to keep it in the same time
  units as the input data. If this parameter is set to `TRUE` the
  incidence rates are calculated per year. If given as a lubridate
  period string e.g. "1 week" then the incidence is calculated over that
  time period.

## Value

a dataframe with incidence rates per unit capita. A dataframe containing
the following columns:

- population (positive_integer) - Size of population

- count (positive_integer) - Positive case counts associated with the
  specified time frame

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

Any grouping allowed.

## Examples

``` r
data = example_england_covid_by_age()
demog = ukc19::uk_population_2019_by_5yr_age

data %>%
  normalise_count(demog) %>%
  dplyr::glimpse()
#> Rows: 26,790
#> Columns: 12
#> Groups: class [19]
#> $ class            <fct> 00_04, 05_09, 10_14, 15_19, 20_24, 25_29, 30_34, 35_3…
#> $ time             <t[day]> 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32…
#> $ code             <chr> "E92000001", "E92000001", "E92000001", "E92000001", "…
#> $ date             <date> 2020-01-30, 2020-01-30, 2020-01-30, 2020-01-30, 2020…
#> $ name             <chr> "England", "England", "England", "England", "England"…
#> $ codeType         <chr> "CTRY20", "CTRY20", "CTRY20", "CTRY20", "CTRY20", "CT…
#> $ count            <int> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
#> $ denom            <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
#> $ population       <dbl> 3299637, 3538206, 3354246, 3090232, 3487863, 3801409,…
#> $ count.per_capita <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.028…
#> $ population_unit  <dbl> 1e+05, 1e+05, 1e+05, 1e+05, 1e+05, 1e+05, 1e+05, 1e+0…
#> $ time_unit        <Period> 1d 0H 0M 0S, 1d 0H 0M 0S, 1d 0H 0M 0S, 1d 0H 0M 0S…
```
