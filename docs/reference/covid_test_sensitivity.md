# Test sensitivity of PCR tests

The probability of detecting COVID using PCR given time since infection,
based on Binny et al 2023.

## Usage

``` r
data(covid_test_sensitivity)
```

## Format

A dataframe containing the following columns:

- tau (numeric) - the time column

- probability (numeric) - the probability column

- boot (integer) - the boot column

Must be grouped by: boot (and other groupings allowed).

5100 rows and 3 columns

## References

<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9384503/>
