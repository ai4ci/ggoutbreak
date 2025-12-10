# The delayed observation dataset

This simulates what might be observed in an outbreak if there was on
average a 5 day delay on the reporting of hospital admissions. The
configuration of the outbreak is the same as
[`ggoutbreak::test_bpm`](https://ai4ci.github.io/ggoutbreak/reference/test_bpm.md),
but this is summary data that describes the whole history of admissions
that were observed, when observed at any given time point. This is a
triangular set of data where the counts are right censored by the
observation time.

## Usage

``` r
data(test_delayed_observation)
```

## Format

A dataframe containing the following columns:

- statistic (character) - the statistic column (only admissions)

- obs_time (as.time_period) - the time of observation of the time-series

- time (as.time_period) - the time of the data point in the time-series

- count (integer) - the count of admissions

Grouped by: obs_time + statistic.

3321 rows and 4 columns
