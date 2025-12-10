# Germany demographics

Derived from the Robert Koch Survstat service by comparing counts and
incidence rates.

## Usage

``` r
data(germany_demographics)
```

## Format

A dataframe containing the following columns:

- class
  (enum(`0–14`,`15–19`,`20–24`,`25–29`,`30–39`,`40–49`,`50–59`,`60–69`,`70–79`,`80+`,
  .ordered=TRUE)) - the class column

- population (integer) - the population column

Must be grouped by: class (and other groupings allowed).

No default value.

10 rows and 2 columns
