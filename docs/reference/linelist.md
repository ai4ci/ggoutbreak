# Coerce an object to a `ggoutbreak` compatible case linelist.

Coerce an object to a `ggoutbreak` compatible case linelist.

## Usage

``` r
linelist(x, ...)
```

## Arguments

- x:

  An object to coerce

- ...:

  Named arguments passed on to
  [`as.time_period`](https://ai4ci.github.io/ggoutbreak/reference/as.time_period.md)

  `unit`

  :   the length of one unit of time. This will be either a integer
      number of days, or a specification such as "1 week", or another
      `time_period`. If `x` is a `time_period`, and the unit is
      different to that of `x` this will return a rescaled `time_period`
      using the new units.

  `start_date`

  :   the zero time date as something that can be coerced to a date. If
      the `x` input is already a `time_period` and this is different to
      its `start_date` then `x` will be recalibrated to use the new
      start date.

  `anchor`

  :   only relevant if `x` is a vector of dates, this is a date, or
      `"start"` or `"end"` or a weekday name e.g. `"mon"`. With the
      vector of dates in `x` it will use this anchor to find a reference
      date for the time-series. If not provided then the current
      defaults will be used. (see
      [`set_defaults()`](https://ai4ci.github.io/ggoutbreak/reference/set_defaults.md))

## Value

minimally a time stamped linelist dataframe A dataframe containing the
following columns:

- time (ggoutbreak::time_period) - A set of events with a timestamp as a
  `time_period`

Any grouping allowed.

## Examples

``` r
(Sys.Date()+stats::runif(100)*7) %>% linelist()
#> # A tibble: 100 × 1
#>         time
#>    <t[week]>
#>  1     310.9
#>  2       311
#>  3     310.9
#>  4       311
#>  5     310.7
#>  6       311
#>  7     311.1
#>  8     310.8
#>  9     310.2
#> 10     310.8
#> # ℹ 90 more rows
```
