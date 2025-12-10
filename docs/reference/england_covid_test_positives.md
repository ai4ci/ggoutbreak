# Weekly England COVID test positives by age group including testing effort

An age group stratified dataset from

## Usage

``` r
data(england_covid_test_positives)
```

## Format

A dataframe containing the following columns:

- class (character) - the age group

- date (date) - the start date of a week

- count (numeric) - the count of COVID positives

- denom (numeric) - the number of COVID tests performed for that age
  group that week

- population (numeric) - the size of the population in this age group

- time (time_period) - the time column (weekly)

Must be grouped by: class (and other groupings allowed).

No default value.

1050 rows and 6 columns

## Details

- the coronavirus.gov.uk site for positive cases aggregated to 10 year
  age groups and by weekly time.

- NHS test and trace date which reported regional by age group testing
  effort aggregated to country level.

- ONS 2021 census population aggregated to 10 year age groups.
