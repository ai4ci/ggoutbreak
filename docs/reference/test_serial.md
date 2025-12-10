# A serial interval estimated from simulated data

This serial interval is resampled from the first 1000 patients in the
`test_bpm` dataset for whom both infector and infectee has symptoms.
These patients are generated with a symptom delay of mean 6 days and SD
2 from infection (discrete under-dispersed gamma) and an infectivity
profile with mean 5 days and SD 2 as defined in `example_ip()` dataset.
This serial interval is relevant to the estimation of \$R_t\$ from
symptomatic case counts in the `test_bpm` dataset but includes negative
times, and cannot be used with `EpiEstim`.

## Usage

``` r
data(test_serial)
```

## Format

A dataframe containing the following columns:

- tau (numeric) - the time delay between symptoms in infector and
  infectee

- a0 (numeric) - the a0 column

- a1 (numeric) - the a1 column

- probability (numeric) - the probability column

- boot (integer) - the boot column

Minimally grouped by: boot (and other groupings allowed).

2166 rows and 5 columns
