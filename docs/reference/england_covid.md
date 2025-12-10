# Daily COVID-19 case counts by age group in England

A dataset of the daily count of COVID-19 cases by age group in England
downloaded from the UKHSA coronavirus API, and formatted for use in
`ggoutbreak`. A denominator is calculated which is the overall positive
count for all age groups. This data set can be used to calculate
group-wise incidence and absolute growth rates and group wise
proportions and relative growth rates by age group.

## Usage

``` r
data(england_covid)
```

## Format

A dataframe containing the following columns:

- date (as.Date) - the date column

- class
  (enum(`00_04`,`05_09`,`10_14`,`15_19`,`20_24`,`25_29`,`30_34`,`35_39`,`40_44`,`45_49`,`50_54`,`55_59`,`60_64`,`65_69`,`70_74`,`75_79`,`80_84`,`85_89`,`90+`)) -
  the class column

- count (numeric) - the test positives for each age group

- denom (numeric) - the test positives across all age groups

- time (time_period) - the time column

Must be grouped by: class (and other groupings allowed).

No default value.

26790 rows and 5 columns

## Details

You may want `england_covid_test_positives` instead which includes the
population denominator. The denominator here is the total number of
positive tests across all age groups and not the number of tests taken
or population size.
