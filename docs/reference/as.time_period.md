# Time period S3 class methods

Time periods are just a zero based numeric representation of dates with
a time unit baked in. This allows variable length periods (e.g. days or
weeks), and fractional days to be represented in a consistent(ish) way
between things that want to deal in dates (like ggplot) and things that
want to deal in numbers (like model fitting)

## Usage

``` r
as.time_period(x, ...)

# S3 method for class 'time_period'
as.time_period(x, unit = NULL, start_date = NULL, ...)

# S3 method for class 'Date'
as.time_period(x, unit = NULL, anchor = NULL, ...)

# S3 method for class 'numeric'
as.time_period(x, unit = NULL, start_date = NULL, ...)

# S3 method for class 'grates_epiweek'
as.time_period(x, ...)

# S3 method for class 'grates_isoweek'
as.time_period(x, ...)

# S3 method for class 'grates_period'
as.time_period(x, ...)

# S3 method for class 'time_period'
seq(from, to = from, ...)

is.time_period(x)

date_to_time(dates, unit = NULL, start_date = NULL)

time_to_date(
  timepoints,
  unit = attr(timepoints, "unit"),
  start_date = attr(timepoints, "start_date")
)
```

## Arguments

- x:

  a vector of dates, numbers (may be integer or real) or a `time_period`
  to convert to a `time_period`

- ...:

  arguments passed to or from methods.

- unit:

  the length of one unit of time. This will be either a integer number
  of days, or a specification such as "1 week", or another
  `time_period`. If `x` is a `time_period`, and the unit is different to
  that of `x` this will return a rescaled `time_period` using the new
  units.

- start_date:

  the zero time date as something that can be coerced to a date. If the
  `x` input is already a `time_period` and this is different to its
  `start_date` then `x` will be recalibrated to use the new start date.

- anchor:

  only relevant if `x` is a vector of dates, this is a date, or
  `"start"` or `"end"` or a weekday name e.g. `"mon"`. With the vector
  of dates in `x` it will use this anchor to find a reference date for
  the time-series. If not provided then the current defaults will be
  used. (see
  [`set_defaults()`](https://ai4ci.github.io/ggoutbreak/reference/set_defaults.md))

- from, to:

  the starting and (maximal) end values of the sequence. Of length `1`
  unless just `from` is supplied as an unnamed argument.

- dates:

  a vector of dates to convert to a `time_period`

- timepoints:

  a `time_period` vector to convert to a set of dates.

## Value

a vector of class `time_period`

## Functions

- `seq(time_period)`: Create a sequence using `time_period`s

- `is.time_period()`: Check is a `time_period`

- `date_to_time()`: Convert a set of dates to numeric timepoints

- `time_to_date()`: Convert a set of time points to dates

## Unit tests


    tmp = as.time_period(grates::as_epiweek("2019-W12", format = "yearweek")+0:2)
    testthat::expect_equal(
      lubridate::epiweek(as.Date(tmp)),
      c(12, 13, 14)
    )

    tmp = as.time_period(grates::as_isoweek("2019-W12", format = "yearweek")+0:2)
    testthat::expect_equal(
      lubridate::isoweek(as.Date(tmp)),
      c(12, 13, 14)
    )

    x = as.time_period(as.Date("2025-01-01")+0:2, anchor="start", unit="1 day")
    y = as.time_period(as.Date("2025-01-07")+0:2, anchor="start", unit="1 day")
    testthat::expect_equal(as.numeric(c(x, y)), c(0, 1, 2, 6, 7, 8))
    testthat::expect_equal(as.numeric(c(y, x)), c(0, 1, 2, -6, -5, -4))

    z = as.time_period(as.Date("2025-01-01")+0:2*7, anchor="start", unit="1 week")
    testthat::expect_equal(as.numeric(c(x, z)), c(0, 1, 2, 0, 7, 14))
    testthat::expect_equal(
      as.numeric(c(z, x)),
      c(0, 1, 2, 0, 0.142857142857143, 0.285714285714286)
    )

    testthat::expect_equal(
      as.Date(c(y, z)),
      structure(c(20095, 20096, 20097, 20089, 20096, 20103), class = "Date")
    )

    seq(x[[1]], y[[1]])

## Examples

``` r
#' # 100 weeks from 2020-01-01

tmp = as.time_period(0:100, 7, "2020-01-01")
as.Date(tmp)
#>   [1] "2020-01-01" "2020-01-08" "2020-01-15" "2020-01-22" "2020-01-29"
#>   [6] "2020-02-05" "2020-02-12" "2020-02-19" "2020-02-26" "2020-03-04"
#>  [11] "2020-03-11" "2020-03-18" "2020-03-25" "2020-04-01" "2020-04-08"
#>  [16] "2020-04-15" "2020-04-22" "2020-04-29" "2020-05-06" "2020-05-13"
#>  [21] "2020-05-20" "2020-05-27" "2020-06-03" "2020-06-10" "2020-06-17"
#>  [26] "2020-06-24" "2020-07-01" "2020-07-08" "2020-07-15" "2020-07-22"
#>  [31] "2020-07-29" "2020-08-05" "2020-08-12" "2020-08-19" "2020-08-26"
#>  [36] "2020-09-02" "2020-09-09" "2020-09-16" "2020-09-23" "2020-09-30"
#>  [41] "2020-10-07" "2020-10-14" "2020-10-21" "2020-10-28" "2020-11-04"
#>  [46] "2020-11-11" "2020-11-18" "2020-11-25" "2020-12-02" "2020-12-09"
#>  [51] "2020-12-16" "2020-12-23" "2020-12-30" "2021-01-06" "2021-01-13"
#>  [56] "2021-01-20" "2021-01-27" "2021-02-03" "2021-02-10" "2021-02-17"
#>  [61] "2021-02-24" "2021-03-03" "2021-03-10" "2021-03-17" "2021-03-24"
#>  [66] "2021-03-31" "2021-04-07" "2021-04-14" "2021-04-21" "2021-04-28"
#>  [71] "2021-05-05" "2021-05-12" "2021-05-19" "2021-05-26" "2021-06-02"
#>  [76] "2021-06-09" "2021-06-16" "2021-06-23" "2021-06-30" "2021-07-07"
#>  [81] "2021-07-14" "2021-07-21" "2021-07-28" "2021-08-04" "2021-08-11"
#>  [86] "2021-08-18" "2021-08-25" "2021-09-01" "2021-09-08" "2021-09-15"
#>  [91] "2021-09-22" "2021-09-29" "2021-10-06" "2021-10-13" "2021-10-20"
#>  [96] "2021-10-27" "2021-11-03" "2021-11-10" "2021-11-17" "2021-11-24"
#> [101] "2021-12-01"

range(tmp)
#> time unit: week, origin: 2020-01-01 (a Wednesday)
#> 0 100
min(tmp)
#> time unit: week, origin: 2020-01-01 (a Wednesday)
#> 0
tmp2 = as.integer(as.Date(tmp))
# testthat::expect_true(all(na.omit(tmp2-lag(tmp2)) == 7))

tmp2 = as.time_period(0:23, 1/24, "2020-01-01")
as.POSIXct(tmp2)
#>  [1] "2020-01-01 00:00:00 GMT" "2020-01-01 01:00:00 GMT"
#>  [3] "2020-01-01 02:00:00 GMT" "2020-01-01 03:00:00 GMT"
#>  [5] "2020-01-01 04:00:00 GMT" "2020-01-01 05:00:00 GMT"
#>  [7] "2020-01-01 06:00:00 GMT" "2020-01-01 07:00:00 GMT"
#>  [9] "2020-01-01 08:00:00 GMT" "2020-01-01 09:00:00 GMT"
#> [11] "2020-01-01 10:00:00 GMT" "2020-01-01 11:00:00 GMT"
#> [13] "2020-01-01 12:00:00 GMT" "2020-01-01 13:00:00 GMT"
#> [15] "2020-01-01 14:00:00 GMT" "2020-01-01 15:00:00 GMT"
#> [17] "2020-01-01 16:00:00 GMT" "2020-01-01 17:00:00 GMT"
#> [19] "2020-01-01 18:00:00 GMT" "2020-01-01 19:00:00 GMT"
#> [21] "2020-01-01 20:00:00 GMT" "2020-01-01 21:00:00 GMT"
#> [23] "2020-01-01 22:00:00 GMT" "2020-01-01 23:00:00 GMT"

# convert timeseries to new "unit"
tmp = as.time_period(0:100, 7, "2020-01-01")
tmp2 = as.time_period(tmp,1)
testthat::expect_equal(as.numeric(tmp2), 0:100*7)

# 100 weeks from 2020-01-01

tmp = as.time_period(0:100, 7, "2020-01-01")
as.Date(tmp)
#>   [1] "2020-01-01" "2020-01-08" "2020-01-15" "2020-01-22" "2020-01-29"
#>   [6] "2020-02-05" "2020-02-12" "2020-02-19" "2020-02-26" "2020-03-04"
#>  [11] "2020-03-11" "2020-03-18" "2020-03-25" "2020-04-01" "2020-04-08"
#>  [16] "2020-04-15" "2020-04-22" "2020-04-29" "2020-05-06" "2020-05-13"
#>  [21] "2020-05-20" "2020-05-27" "2020-06-03" "2020-06-10" "2020-06-17"
#>  [26] "2020-06-24" "2020-07-01" "2020-07-08" "2020-07-15" "2020-07-22"
#>  [31] "2020-07-29" "2020-08-05" "2020-08-12" "2020-08-19" "2020-08-26"
#>  [36] "2020-09-02" "2020-09-09" "2020-09-16" "2020-09-23" "2020-09-30"
#>  [41] "2020-10-07" "2020-10-14" "2020-10-21" "2020-10-28" "2020-11-04"
#>  [46] "2020-11-11" "2020-11-18" "2020-11-25" "2020-12-02" "2020-12-09"
#>  [51] "2020-12-16" "2020-12-23" "2020-12-30" "2021-01-06" "2021-01-13"
#>  [56] "2021-01-20" "2021-01-27" "2021-02-03" "2021-02-10" "2021-02-17"
#>  [61] "2021-02-24" "2021-03-03" "2021-03-10" "2021-03-17" "2021-03-24"
#>  [66] "2021-03-31" "2021-04-07" "2021-04-14" "2021-04-21" "2021-04-28"
#>  [71] "2021-05-05" "2021-05-12" "2021-05-19" "2021-05-26" "2021-06-02"
#>  [76] "2021-06-09" "2021-06-16" "2021-06-23" "2021-06-30" "2021-07-07"
#>  [81] "2021-07-14" "2021-07-21" "2021-07-28" "2021-08-04" "2021-08-11"
#>  [86] "2021-08-18" "2021-08-25" "2021-09-01" "2021-09-08" "2021-09-15"
#>  [91] "2021-09-22" "2021-09-29" "2021-10-06" "2021-10-13" "2021-10-20"
#>  [96] "2021-10-27" "2021-11-03" "2021-11-10" "2021-11-17" "2021-11-24"
#> [101] "2021-12-01"

range(tmp)
#> time unit: week, origin: 2020-01-01 (a Wednesday)
#> 0 100
min(tmp)
#> time unit: week, origin: 2020-01-01 (a Wednesday)
#> 0
tmp2 = as.integer(as.Date(tmp))
# testthat::expect_true(all(na.omit(tmp2-lag(tmp2)) == 7))

tmp2 = as.time_period(0:23, 1/24, "2020-01-01")
as.POSIXct(tmp2)
#>  [1] "2020-01-01 00:00:00 GMT" "2020-01-01 01:00:00 GMT"
#>  [3] "2020-01-01 02:00:00 GMT" "2020-01-01 03:00:00 GMT"
#>  [5] "2020-01-01 04:00:00 GMT" "2020-01-01 05:00:00 GMT"
#>  [7] "2020-01-01 06:00:00 GMT" "2020-01-01 07:00:00 GMT"
#>  [9] "2020-01-01 08:00:00 GMT" "2020-01-01 09:00:00 GMT"
#> [11] "2020-01-01 10:00:00 GMT" "2020-01-01 11:00:00 GMT"
#> [13] "2020-01-01 12:00:00 GMT" "2020-01-01 13:00:00 GMT"
#> [15] "2020-01-01 14:00:00 GMT" "2020-01-01 15:00:00 GMT"
#> [17] "2020-01-01 16:00:00 GMT" "2020-01-01 17:00:00 GMT"
#> [19] "2020-01-01 18:00:00 GMT" "2020-01-01 19:00:00 GMT"
#> [21] "2020-01-01 20:00:00 GMT" "2020-01-01 21:00:00 GMT"
#> [23] "2020-01-01 22:00:00 GMT" "2020-01-01 23:00:00 GMT"

# convert timeseries to new "unit"
tmp = as.time_period(0:100, 7, "2020-01-01")
tmp2 = as.time_period(tmp,1)
testthat::expect_equal(as.numeric(tmp2), 0:100*7)

# Time to date
times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
dates = time_to_date(times)

# Date to time
times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
dates = time_to_date(times)
```
