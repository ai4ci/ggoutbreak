# Places a set of dates within a regular time series

The counterpart to date_seq_dates(). Take an original set of data and
place it within a regular time series where the periodicity of the time
series may be expressed as numbers of days, weeks, months quarters, or
years, and the periods are defined by an anchoring date, day of the week
or by reference to the start or end of the input dates. This can either
return the periods as dates or factors (e.g. for plotting) or as a
`time_period` for analysis that relies on a numeric representation of
the date or duration from the anchor.

## Usage

``` r
cut_date(
  dates,
  unit,
  anchor = "start",
  output = c("date", "factor", "time_period"),
  dfmt = "%d/%b/%y",
  ifmt = "{start} — {end}",
  ...
)
```

## Arguments

- dates:

  a set of dates

- unit:

  a period e.g. "1 week"

- anchor:

  one of a date, "start" or "end" or a weekday name e.g. "mon" this will
  always be one of the start of the time periods we are cutting into

- output:

  return the result as either a "date" (the default), an ordered
  "factor" with the date ranges as a label, or as a "time_period". The
  result is named with labels referring to the

- dfmt:

  the `strptime` format for the dates in the labels

- ifmt:

  a `sprintf` format for the period label containing `%s` exactly twice.

- ...:

  ignored

## Value

a set of dates, times or a factor level, representing the start of the
period the date falls into, where the period is defined by the duration
and the anchor

## Examples

``` r
dates = as.Date(c("2020-01-01","2020-02-01","2020-01-15","2020-02-03",NA))
fs = date_seq(dates, "2 days")
dates - cut_date(dates, "2 days")
#> Time differences in days
#> 01/Jan/20 — 02/Jan/20 31/Jan/20 — 01/Feb/20 15/Jan/20 — 16/Jan/20 
#>                     0                     1                     0 
#> 02/Feb/20 — 03/Feb/20               Unknown 
#>                     1                    NA 
cut_date(dates,unit="2 days", output="time_period")
#> time unit: 2 days, origin: 2020-01-01 (a Wednesday)
#> 0 15 7 16 NA

# A weekly set of dates:
dates2 = Sys.Date() + floor(stats::runif(50,max=10))*7

# in this specific situation the final date is not truncated because the
# input data is seen as an exact match for the whole output period.
cut_date(dates2, "1 week", "sun", output="factor")
#>  [1] 08/Feb/26 — 14/Feb/26 28/Dec/25 — 03/Jan/26 01/Feb/26 — 07/Feb/26
#>  [4] 04/Jan/26 — 10/Jan/26 28/Dec/25 — 03/Jan/26 11/Jan/26 — 17/Jan/26
#>  [7] 25/Jan/26 — 31/Jan/26 07/Dec/25 — 13/Dec/25 08/Feb/26 — 14/Feb/26
#> [10] 04/Jan/26 — 10/Jan/26 01/Feb/26 — 07/Feb/26 04/Jan/26 — 10/Jan/26
#> [13] 28/Dec/25 — 03/Jan/26 01/Feb/26 — 07/Feb/26 28/Dec/25 — 03/Jan/26
#> [16] 01/Feb/26 — 07/Feb/26 25/Jan/26 — 31/Jan/26 25/Jan/26 — 31/Jan/26
#> [19] 18/Jan/26 — 24/Jan/26 21/Dec/25 — 27/Dec/25 08/Feb/26 — 14/Feb/26
#> [22] 25/Jan/26 — 31/Jan/26 21/Dec/25 — 27/Dec/25 07/Dec/25 — 13/Dec/25
#> [25] 11/Jan/26 — 17/Jan/26 04/Jan/26 — 10/Jan/26 11/Jan/26 — 17/Jan/26
#> [28] 28/Dec/25 — 03/Jan/26 21/Dec/25 — 27/Dec/25 25/Jan/26 — 31/Jan/26
#> [31] 21/Dec/25 — 27/Dec/25 08/Feb/26 — 14/Feb/26 04/Jan/26 — 10/Jan/26
#> [34] 08/Feb/26 — 14/Feb/26 18/Jan/26 — 24/Jan/26 28/Dec/25 — 03/Jan/26
#> [37] 18/Jan/26 — 24/Jan/26 08/Feb/26 — 14/Feb/26 11/Jan/26 — 17/Jan/26
#> [40] 14/Dec/25 — 20/Dec/25 08/Feb/26 — 14/Feb/26 28/Dec/25 — 03/Jan/26
#> [43] 07/Dec/25 — 13/Dec/25 08/Feb/26 — 14/Feb/26 25/Jan/26 — 31/Jan/26
#> [46] 14/Dec/25 — 20/Dec/25 21/Dec/25 — 27/Dec/25 04/Jan/26 — 10/Jan/26
#> [49] 08/Feb/26 — 14/Feb/26 28/Dec/25 — 03/Jan/26
#> 11 Levels: 07/Dec/25 — 13/Dec/25 < ... < 15/Feb/26 — 21/Feb/26
cut_date(dates2, dfmt = "%d/%b", output="factor", unit = "2 weeks", anchor="sun")
#>  [1] 01/Feb — 14/Feb 21/Dec — 03/Jan 01/Feb — 14/Feb 04/Jan — 17/Jan
#>  [5] 21/Dec — 03/Jan 04/Jan — 17/Jan 18/Jan — 31/Jan 07/Dec — 20/Dec
#>  [9] 01/Feb — 14/Feb 04/Jan — 17/Jan 01/Feb — 14/Feb 04/Jan — 17/Jan
#> [13] 21/Dec — 03/Jan 01/Feb — 14/Feb 21/Dec — 03/Jan 01/Feb — 14/Feb
#> [17] 18/Jan — 31/Jan 18/Jan — 31/Jan 18/Jan — 31/Jan 21/Dec — 03/Jan
#> [21] 01/Feb — 14/Feb 18/Jan — 31/Jan 21/Dec — 03/Jan 07/Dec — 20/Dec
#> [25] 04/Jan — 17/Jan 04/Jan — 17/Jan 04/Jan — 17/Jan 21/Dec — 03/Jan
#> [29] 21/Dec — 03/Jan 18/Jan — 31/Jan 21/Dec — 03/Jan 01/Feb — 14/Feb
#> [33] 04/Jan — 17/Jan 01/Feb — 14/Feb 18/Jan — 31/Jan 21/Dec — 03/Jan
#> [37] 18/Jan — 31/Jan 01/Feb — 14/Feb 04/Jan — 17/Jan 07/Dec — 20/Dec
#> [41] 01/Feb — 14/Feb 21/Dec — 03/Jan 07/Dec — 20/Dec 01/Feb — 14/Feb
#> [45] 18/Jan — 31/Jan 07/Dec — 20/Dec 21/Dec — 03/Jan 04/Jan — 17/Jan
#> [49] 01/Feb — 14/Feb 21/Dec — 03/Jan
#> 6 Levels: 07/Dec — 20/Dec < 21/Dec — 03/Jan < ... < 15/Feb — 28/Feb
```
