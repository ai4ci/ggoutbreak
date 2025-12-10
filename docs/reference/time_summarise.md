# Summarise data from a line list to a time-series of counts.

This principally is designed to take a record of single events and
produce a summary time-series count of events by group, class and date.
The default behaviour is to guess the cadence of the input data and
summarise the event line list to a (set of) regular time-series counts
for use in incidence and growth rate estimates.

## Usage

``` r
time_summarise(
  df = i_dated,
  unit,
  anchor = "start",
  rectangular = FALSE,
  ...,
  .fill = list(count = 0)
)
```

## Arguments

- df:

  a line list of data you want to summarise, optionally grouped. If this
  is grouped then each group is treated independently. The remaining
  columns must contain a `date` column and may contain a `class` column.
  If a `count` column is present the counts will be summed, otherwise
  each individual row will be counted as a single event (as a line list)

- unit:

  a period e.g. "1 week"

- anchor:

  one of a date, "start" or "end" or a weekday name e.g. "mon" this will
  always be one of the start of the time periods we are cutting into

- rectangular:

  should the resulting time series be the same length for all groups?
  This is only the case if you can be sure that your data is complete
  for all subgroups, otherwise missing data will be treated as zero
  counts. This is important if leading and trailing missing data in one
  subgroup can be due to a reporting delay in that subgroup, in which
  case a rectangular time series will erroneously fill in zero counts
  for this missing data.

- ...:

  a specification for a dplyr::summary(...) - optional, and if not
  provided a `count = dplyr::n()` or a `count = sum(count)` is
  performed.

- .fill:

  a list similar to
  [`tidyr::complete`](https://tidyr.tidyverse.org/reference/complete.html)
  for default values to fill variables with.

## Value

The output depends on whether or not the input was grouped and had a
`class` column. The most detailed output will be:

A dataframe containing the following columns:

- denom (positive_integer) - Total test counts associated with the
  specified time frame

- count (positive_integer) - Positive case counts associated with the
  specified time frame

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

Any grouping allowed.

or a more minimal output if the input is only a plain list of dated
events:

A dataframe containing the following columns:

- count (positive_integer) - Positive case counts associated with the
  specified time frame

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

Any grouping allowed.

## Details

If the data is given with a `class` column the time series are
interpreted as having a denominator, consisting of all the different
classes within a time period. This may be subtypes (e.g. variants,
serotypes) or markers for test positivity. In either case the resulting
time series will have counts for all classes and denominators for the
combination.

There is flexibility for other kinds of summarisation if the raw data is
not count based (e.g. means of continuous variables) but in this case a
the `slider` package is usually going to be better, as time summarise
will only look at non overlapping time periods with fixed lengths.

There is another use case where an existing timeseries on a particular
frequency is aggregated to another less frequent basis (e.g. moving from
a daily timeseries to a weekly one). In this case the input will contain
a `count` column. In this mode no checks are made that the more frequent
events are all present before summarisation so the result may include
different numbers of input periods (e.g. going from weeks to months may
be 4 or 5 weeks in each month). If a `class` column is present the a
classwise denominator is calculated

## Examples

``` r
# a set of random dates with a class column:
input = dplyr::tibble(
  class = rep(c("A","B"),1000),
  date = as.Date("2020-01-01")+sample.int(100,2000,TRUE)
)

# summarise daily counts, including denominators:
daily = time_summarise(input, unit="1 day") %>% dplyr::glimpse()
#> Rows: 200
#> Columns: 4
#> Groups: class [2]
#> $ class <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",…
#> $ time  <t[day]> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
#> $ count <int> 8, 15, 8, 11, 11, 3, 11, 10, 7, 15, 9, 14, 8, 17, 5, 13, 6, 8, 5…
#> $ denom <int> 15, 25, 17, 17, 18, 9, 16, 17, 17, 30, 20, 24, 17, 25, 16, 25, 1…

# summarise weekly counts, for sample data:
time_summarise(input, unit="1 week") %>% dplyr::glimpse()
#> Rows: 30
#> Columns: 4
#> Groups: class [2]
#> $ class <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",…
#> $ time  <t[week]> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0, 1, 2, 3…
#> $ count <int> 67, 80, 57, 72, 88, 64, 66, 72, 63, 72, 65, 75, 74, 71, 14, 50, …
#> $ denom <int> 117, 150, 129, 132, 167, 129, 137, 155, 137, 148, 142, 155, 144,…

# summarise daily counts into weekly:
time_summarise(daily, unit="1 week") %>% dplyr::glimpse()
#> N.B. time_summarise ignores existing time_period columns 
#> (N.B. this message will only be displayed once.)
#> Rows: 30
#> Columns: 4
#> Groups: class [2]
#> $ class <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",…
#> $ time  <t[week]> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0, 1, 2, 3…
#> $ count <int> 67, 80, 57, 72, 88, 64, 66, 72, 63, 72, 65, 75, 74, 71, 14, 50, …
#> $ denom <int> 117, 150, 129, 132, 167, 129, 137, 155, 137, 148, 142, 155, 144,…
```
