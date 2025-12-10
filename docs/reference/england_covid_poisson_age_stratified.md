# The England COVID-19 age stratified poisson model dataset

This is the output of the following estimator, and is here to speed up
some examples:

## Usage

``` r
england_covid_poisson_age_stratified()
```

## Value

A dataframe containing the following columns:

- class
  (enum(`00_04`,`05_09`,`10_14`,`15_19`,`20_24`,`25_29`,`30_34`,`35_39`,`40_44`,`45_49`,`50_54`,`55_59`,`60_64`,`65_69`,`70_74`,`75_79`,`80_84`,`85_89`,`90+`)) -
  the class column

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

Minimally grouped by: class (and other groupings allowed).

26790 rows and 20 columns

## Details

    england_covid %>% ggoutbreak::poisson_locfit_model(window=14)
