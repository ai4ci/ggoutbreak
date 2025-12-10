# Expand a date vector to the full range of possible dates

Derive from a vector of observation dates, a complete ordered sequence
of periods in a regular time series, where the length of the periods is
specified, as a number of days, weeks, years etcetera. E.g. this can
convert a random set of dates to a ordered complete list of 1 week
intervals (or 2 month intervals) spanning the same range as the dates.
This has some interesting problems regarding where to put breaks within
a month or week. Often this is either based on a specific date (e.g.
yearly periods starting at `2020-01-01`) or a day of week (e.g. 2 weekly
periods staring on a Sunday) or maybe relative to the input time series
(weekly ending on the last date of the data). There is also a problem
when we consider data that may have incomplete starting and end periods,
which may not be comparable to other periods, and we may need to exclude
these from the result.

## Usage

``` r
# S3 method for class 'Date'
date_seq(x, period = .day_interval(x), anchor = "start", complete = FALSE, ...)
```

## Arguments

- x:

  a vector of dates, possibly including NA values

- period:

  the gap between observations as a number of days or as a natural
  language definition of the period such as "1 week", '2 weeks', '1
  month', etcetera. If not given this will be derived from the dates.

- anchor:

  defines a day that appears in the sequence (if it were to extend that
  far). Given either as a date, or "start", "end" or a day of the week,
  e.g. "mon".

- complete:

  truncate incomplete start and end periods

- ...:

  ignored

## Value

a vector of dates for regular periods between the minimum and maximum of
dates, with the boundaries defined by the anchor.

## Examples

``` r
date_seq(as.Date(c("2020-01-01","2020-02-01","2020-01-15","2020-02-01",NA)), "2 days")
#>  [1] "2020-01-01" "2020-01-03" "2020-01-05" "2020-01-07" "2020-01-09"
#>  [6] "2020-01-11" "2020-01-13" "2020-01-15" "2020-01-17" "2020-01-19"
#> [11] "2020-01-21" "2020-01-23" "2020-01-25" "2020-01-27" "2020-01-29"
#> [16] "2020-01-31"
```
