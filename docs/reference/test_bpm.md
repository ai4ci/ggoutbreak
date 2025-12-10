# An example of the linelist output of the branching process model simulation

This is generated using the `example_ip()` infectivity profile and also
includes a delay to symptom onset which is a random gamma distributed
quantity with mean of 6 and standard deviation of 2

## Usage

``` r
data(test_bpm)
```

## Format

A dataframe containing the following columns:

- time (as.time_period) - the time column

- id (integer) - an id per individual

- generation_interval (numeric) - the generation_interval column

- infector (integer) - the infector id

- generation (numeric) - the generation column

- symptom_onset (logical) - the flag for onset of symptoms

- symptom_onset_delay (numeric) - the time to onset of symptoms from
  infection

- symptom_onset_time (as.time_period) - the time of symptom onset

333126 rows and 8 columns
