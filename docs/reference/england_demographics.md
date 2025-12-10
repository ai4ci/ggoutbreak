# England demographics

Population counts by 5 year age group for England only from the 2021
census.

## Usage

``` r
data(england_demographics)
```

## Format

A dataframe containing the following columns:

- class
  (enum(`00_04`,`05_09`,`10_14`,`15_19`,`20_24`,`25_29`,`30_34`,`35_39`,`40_44`,`45_49`,`50_54`,`55_59`,`60_64`,`65_69`,`70_74`,`75_79`,`80_84`,`85_89`,`90+`)) -
  the class column

- population (numeric) - the population count column

- baseline_proportion (numeric) - the baseline proportion is the
  proportion this age group makes up of the total.

Must be grouped by: class (and other groupings allowed).

No default value.

19 rows and 3 columns

## Source

https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021/census2021/census2021firstresultsenglandwales1.xlsx
