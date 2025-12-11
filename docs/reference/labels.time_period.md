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
#>  [1] "11/Dec — 17/Dec" "18/Dec — 24/Dec" "25/Dec — 31/Dec" "01/Jan — 07/Jan"
#>  [5] "08/Jan — 14/Jan" "15/Jan — 21/Jan" "22/Jan — 28/Jan" "29/Jan — 04/Feb"
#>  [9] "05/Feb — 11/Feb" "12/Feb — 18/Feb" "19/Feb — 25/Feb"
labels(eg, ifmt="{start}", dfmt="%d/%b/%y")
#>  [1] "11/Dec/25" "18/Dec/25" "25/Dec/25" "01/Jan/26" "08/Jan/26" "15/Jan/26"
#>  [7] "22/Jan/26" "29/Jan/26" "05/Feb/26" "12/Feb/26" "19/Feb/26"
labels(eg, ifmt="until {end}", dfmt="%d %b %Y")
#>  [1] "until 17 Dec 2025" "until 24 Dec 2025" "until 31 Dec 2025"
#>  [4] "until 07 Jan 2026" "until 14 Jan 2026" "until 21 Jan 2026"
#>  [7] "until 28 Jan 2026" "until 04 Feb 2026" "until 11 Feb 2026"
#> [10] "until 18 Feb 2026" "until 25 Feb 2026"

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
