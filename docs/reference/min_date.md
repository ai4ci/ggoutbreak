# The minimum of a set of dates

`min.Date` returns an integer and `Inf` for a set of `NA` dates. This is
usually inconvenient.

## Usage

``` r
min_date(x, ...)
```

## Arguments

- x:

  a vector of dates

- ...:

  ignored

## Value

a date. `9999-12-31` if there is no well defined minimum.

## Examples

``` r
min_date(NA)
#> [1] "9999-12-31"
```
