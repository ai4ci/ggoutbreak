# Set or reset the default origin and unit for time periods

This function is generally not needed, and will be called automatically
when the first date conversion is performed. If no other information is
given then the default origin will be decided by the start of the first
use of the `time_period` class in a session. This helps to keep defaults
consistent during a single run, if they are not specified.

## Usage

``` r
set_defaults(start_date, unit)

with_defaults(start_date, unit, expr)

set_default_start(date)

set_default_unit(unit)
```

## Arguments

- start_date:

  the zero time date as something that can be coerced to a date. If the
  `x` input is already a `time_period` and this is different to its
  `start_date` then `x` will be recalibrated to use the new start date.

- unit:

  the length of one unit of time. This will be either a integer number
  of days, or a specification such as "1 week", or another
  `time_period`. If `x` is a `time_period`, and the unit is different to
  that of `x` this will return a rescaled `time_period` using the new
  units.

- expr:

  an expression to evaluate with the defaults set to the provided
  values.

## Value

depending on the methods the original default start date / the original
default unit, a list of both or the result of evaluating the expression
`expr`

## Functions

- `with_defaults()`: Set defaults temporarily and execute expression

- `set_default_unit()`: Set the default unit only

## Examples

``` r
# set default origin and cadence:
old = set_defaults("2025-01-01", "1 week")

# this sets the default for interpreting underqualified time_periods:
print(as.time_period(1:10))
#> time unit: week, origin: 2025-01-01 (a Wednesday)
#> 1 2 3 4 5 6 7 8 9 10

# The default can always be overridden on a case by case basis:
print(as.time_period(1:10, unit="1 day"))
#> time unit: day, origin: 2025-01-01 (a Wednesday)
#> 1 2 3 4 5 6 7 8 9 10

# or for a whole expression:
with_defaults("2020-01-01", "1 day", {
  print(as.time_period(1:10))
})
#> time unit: day, origin: 2020-01-01 (a Wednesday)
#> 1 2 3 4 5 6 7 8 9 10

# components can be changed individually, firstly origin:
set_default_start("2025-01-01")
#> [1] "2025-01-01"
print(as.time_period(1:10))
#> time unit: week, origin: 2025-01-01 (a Wednesday)
#> 1 2 3 4 5 6 7 8 9 10

# now cadence:
set_default_unit("1 day")
#> [1] "7d 0H 0M 0S"
print(as.time_period(1:10))
#> time unit: day, origin: 2025-01-01 (a Wednesday)
#> 1 2 3 4 5 6 7 8 9 10

# clear the values:
set_defaults(NULL,NULL)

# A sufficiently qualified call will set the defaults:
defined = as.time_period(as.Date("2024-01-01")+0:10*7, anchor="start")
#> No `unit` specified. Inferring default from input: week (N.B. use `set_default_unit(...)` to change) 
#> (N.B. this message will only be displayed once.)
inherit = as.time_period(0:10)

all(defined == inherit)
#> [1] TRUE

# restoring the original values (which might be null)
set_defaults(old)
```
