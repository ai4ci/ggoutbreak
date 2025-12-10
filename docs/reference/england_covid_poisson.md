# The England COVID-19 poisson model dataset

This is the output of the following estimator, and is here to speed up
some examples:

## Usage

``` r
england_covid_proportion_age_stratified()
```

## Value

A dataframe containing the following columns:

- time (as.time_period) - the time column

- incidence.fit (numeric) - the incidence.fit column

- incidence.se.fit (numeric) - the incidence.se.fit column

- incidence.0.025 (numeric) - the incidence.0.025 column

- incidence.0.05 (numeric) - the incidence.0.05 column

- incidence.0.25 (numeric) - the incidence.0.25 column

- incidence.0.5 (numeric) - the incidence.0.5 column

- incidence.0.75 (numeric) - the incidence.0.75 column

- incidence.0.95 (numeric) - the incidence.0.95 column

- incidence.0.975 (numeric) - the incidence.0.975 column

- growth.fit (numeric) - the growth.fit column

- growth.se.fit (numeric) - the growth.se.fit column

- growth.0.025 (numeric) - the growth.0.025 column

- growth.0.05 (numeric) - the growth.0.05 column

- growth.0.25 (numeric) - the growth.0.25 column

- growth.0.5 (numeric) - the growth.0.5 column

- growth.0.75 (numeric) - the growth.0.75 column

- growth.0.95 (numeric) - the growth.0.95 column

- growth.0.975 (numeric) - the growth.0.975 column

Any grouping allowed.

1410 rows and 19 columns

## Details

    england_covid %>% time_aggregate() %>% ggoutbreak::poisson_locfit_model(window=14)
