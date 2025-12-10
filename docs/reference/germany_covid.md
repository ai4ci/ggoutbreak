# Weekly COVID-19 case counts by age group in Germany

A dataset of the weekly count of COVID-19 cases by age group in Germany
downloaded from the Robert Koch Institute `Survstat` service, and
formatted for use in growth rates. A denominator is calculated which is
the overall positive count for all age groups. This data set can be used
to calculate group-wise incidence and absolute growth rates and group
wise proportions and relative growth rates.

## Usage

``` r
data(germany_covid)
```

## Format

A dataframe containing the following columns:

- class
  (enum(`0–14`,`15–19`,`20–24`,`25–29`,`30–39`,`40–49`,`50–59`,`60–69`,`70–79`,`80+`,`Unknown`,
  .ordered=TRUE)) - the age group

- date (as.Date) - the date column

- count (integer) - the test positives for each age group

- time (time_period) - the time column

- denom (integer) - the test positives for all age groups

Must be grouped by: class (and other groupings allowed).

No default value.

2070 rows and 6 columns
