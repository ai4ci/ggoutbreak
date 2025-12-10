# England COVID-19 PCR test positivity

The `coronavirus.gov.uk` dashboard published tests conducted and
positive results as separate data sets for a range of geographies. In
this case the data is combined with testing rate as denominator, and
test positives as count for the whole of England.

## Usage

``` r
data(england_covid_pcr_positivity)
```

## Format

A dataframe containing the following columns:

- date (date) - a daily time series

- time (time_period) - the time column

- count (numeric) - test positives in England on that day

- denom (numeric) - total tests conducted on that day

No mandatory groupings.

No default value.

1413 rows and 4 columns
