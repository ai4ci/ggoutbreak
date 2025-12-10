# Infers a daily baseline population for a timeseries

This function augments any timeseries with a population denominator. The
population data may be static estimates, or a set of estimates at time
points. The population data may be grouped in which case the grouping
might be geographical area or age group or gender for example. The two
inputs must have compatible grouping (i.e. all the groups in the
population data must be present in the timeseries).

## Usage

``` r
infer_population(df = i_timeseries, pop = i_population_data)
```

## Arguments

- df:

  A time series, or a grouped collection of time series. - a dataframe
  with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

- pop:

  The population data must be grouped in the same way as `df`. It might
  also have a `time` column as a `time_period` if the population is not
  static - a dataframe with columns:

  - population (positive_integer) - Size of population

  Any grouping allowed.

## Value

the `df` timeseries with additional `population` column

## Examples

``` r
# The COVID data already has a population column so we are just double checking:
data = example_england_covid_by_age() %>%
  dplyr::rename(pop_old = population)

demog = ukc19::uk_population_2019_by_5yr_age %>% dplyr::group_by(code, name, class)

data %>%
  infer_population(demog) %>%
  dplyr::glimpse()
#> Adding missing grouping variables: `code`, `name`
#> different column groupings in `modelled` and `base` parameters.
#> regrouping `base` data to be compatible with `modelled` grouping
#> Rows: 10,662,420
#> Columns: 12
#> Groups: class [19]
#> $ class      <chr> "00_04", "00_04", "00_04", "00_04", "00_04", "00_04", "00_0…
#> $ time       <t[day]> 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, …
#> $ code.x     <chr> "E92000001", "E92000001", "E92000001", "E92000001", "E92000…
#> $ date       <date> 2020-01-30, 2020-01-30, 2020-01-30, 2020-01-30, 2020-01-30…
#> $ name.x     <chr> "England", "England", "England", "England", "England", "Eng…
#> $ codeType   <chr> "CTRY20", "CTRY20", "CTRY20", "CTRY20", "CTRY20", "CTRY20",…
#> $ count      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ denom      <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
#> $ pop_old    <dbl> 3299637, 3299637, 3299637, 3299637, 3299637, 3299637, 32996…
#> $ code.y     <chr> "E06000001", "E06000002", "E06000003", "E06000004", "E06000…
#> $ name.y     <chr> "Hartlepool", "Middlesbrough", "Redcar and Cleveland", "Sto…
#> $ population <int> 5229, 9583, 7106, 11462, 5790, 7580, 11596, 10575, 8262, 16…
```
