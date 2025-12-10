# Guess the intervals between a sequence of dates:

Guess the intervals between a sequence of dates:

## Usage

``` r
.day_interval(dates)
```

## Arguments

- dates:

  a set of dates

## Value

a natural number of days between ordering

## Unit tests


    r = c(1,rpois(10, 3)+1)
    interval = 2
    dates = as.Date("2025-01-01")+r*interval
    testthat::expect_equal(.day_interval(dates) == interval, TRUE)

    interval = 0.5
    dates = as.Date("2025-01-01")+r*interval
    testthat::expect_equal(.day_interval(dates) == interval, TRUE)
