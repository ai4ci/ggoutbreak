# The maximum of a set of dates

`max.Date` returns an integer and `-Inf` for a set of `NA` dates. This
is usually inconvenient.

## Usage

``` r
max_date(x, ...)
```

## Arguments

- x:

  a vector of dates

- ...:

  ignored

## Value

a date. \`0001-01-01“ if there is no well defined minimum.

## Examples

``` r
max_date(NA)
#> [1] "1-01-01"
```
