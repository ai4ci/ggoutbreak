# Label a time period

Create a set of labels for a time period based on the start and duration
of the period. The format is configurable using the start and end dates
and the `dfmt` and `ifmt` parameters, however if the time period has
names then these are used in preference.

## Usage

``` r
# S3 method for class 'time_period'
labels(
  object,
  ...,
  dfmt = "%d/%b",
  ifmt = "{start} — {end}",
  na.value = "Unknown"
)
```

## Arguments

- object:

  a set of decimal times as a time_period

- ...:

  not used

- dfmt:

  a `strptime` format specification for the format of the date

- ifmt:

  a glue spec referring to `start` and `end` of the period as a
  formatted date

- na.value:

  a label for `NA` times

## Value

a set of character labels for the time

## Examples

``` r
eg = as.time_period(Sys.Date()+0:10*7, unit="1 week", anchor="start")

labels(eg)
#>  [1] "10/Dec — 16/Dec" "17/Dec — 23/Dec" "24/Dec — 30/Dec" "31/Dec — 06/Jan"
#>  [5] "07/Jan — 13/Jan" "14/Jan — 20/Jan" "21/Jan — 27/Jan" "28/Jan — 03/Feb"
#>  [9] "04/Feb — 10/Feb" "11/Feb — 17/Feb" "18/Feb — 24/Feb"
labels(eg, ifmt="{start}", dfmt="%d/%b/%y")
#>  [1] "10/Dec/25" "17/Dec/25" "24/Dec/25" "31/Dec/25" "07/Jan/26" "14/Jan/26"
#>  [7] "21/Jan/26" "28/Jan/26" "04/Feb/26" "11/Feb/26" "18/Feb/26"
labels(eg, ifmt="until {end}", dfmt="%d %b %Y")
#>  [1] "until 16 Dec 2025" "until 23 Dec 2025" "until 30 Dec 2025"
#>  [4] "until 06 Jan 2026" "until 13 Jan 2026" "until 20 Jan 2026"
#>  [7] "until 27 Jan 2026" "until 03 Feb 2026" "until 10 Feb 2026"
#> [10] "until 17 Feb 2026" "until 24 Feb 2026"

# labels retained in constructor:
eg2 = Sys.Date()+0:10*7
names(eg2) = paste0("week ",0:10)
labels(eg2)
#>  [1] "week 0"  "week 1"  "week 2"  "week 3"  "week 4"  "week 5"  "week 6" 
#>  [8] "week 7"  "week 8"  "week 9"  "week 10"
labels(as.time_period(eg2, anchor="start"))
#>  [1] "week 0"  "week 1"  "week 2"  "week 3"  "week 4"  "week 5"  "week 6" 
#>  [8] "week 7"  "week 8"  "week 9"  "week 10"
```
