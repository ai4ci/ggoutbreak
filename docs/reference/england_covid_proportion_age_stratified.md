# The England COVID-19 age stratified proportion model dataset

This is the output of the following estimator, and is here to speed up
some examples. The proportion represented here is the positive tests in
this age group, versus the positive tests in all age groups. It has to
be considered with respect to the overall population.

## Usage

``` r
data(england_covid_proportion_age_stratified)
```

## Format

A dataframe containing the following columns:

- class
  (enum(`00_04`,`05_09`,`10_14`,`15_19`,`20_24`,`25_29`,`30_34`,`35_39`,`40_44`,`45_49`,`50_54`,`55_59`,`60_64`,`65_69`,`70_74`,`75_79`,`80_84`,`85_89`,`90+`)) -
  the class column

- time (as.time_period) - the time column

- proportion.fit (numeric) - the proportion.fit column

- proportion.se.fit (numeric) - the proportion.se.fit column

- proportion.0.025 (numeric) - the proportion.0.025 column

- proportion.0.05 (numeric) - the proportion.0.05 column

- proportion.0.25 (numeric) - the proportion.0.25 column

- proportion.0.5 (numeric) - the proportion.0.5 column

- proportion.0.75 (numeric) - the proportion.0.75 column

- proportion.0.95 (numeric) - the proportion.0.95 column

- proportion.0.975 (numeric) - the proportion.0.975 column

- relative.growth.fit (numeric) - the relative.growth.fit column

- relative.growth.se.fit (numeric) - the relative.growth.se.fit column

- relative.growth.0.025 (numeric) - the relative.growth.0.025 column

- relative.growth.0.05 (numeric) - the relative.growth.0.05 column

- relative.growth.0.25 (numeric) - the relative.growth.0.25 column

- relative.growth.0.5 (numeric) - the relative.growth.0.5 column

- relative.growth.0.75 (numeric) - the relative.growth.0.75 column

- relative.growth.0.95 (numeric) - the relative.growth.0.95 column

- relative.growth.0.975 (numeric) - the relative.growth.0.975 column

Minimally grouped by: class (and other groupings allowed).

26790 rows and 20 columns

## Details

    england_covid %>% ggoutbreak::proportion_locfit_model(window=14)
