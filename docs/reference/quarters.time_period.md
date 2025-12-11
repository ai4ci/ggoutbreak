# Extract Parts of a POSIXt or Date Object

Extract the weekday, month or quarter, or the Julian time (days since
some origin). These are generic functions: the methods for the internal
date-time classes are documented here.

## Usage

``` r
# S3 method for class 'time_period'
quarters(x, ...)
```

## Arguments

- x:

  an object inheriting from class `"POSIXt"` or `"Date"`.

- ...:

  arguments for other methods.

## Value

`weekdays` and `months` return a character vector of names in the locale
in use, i.e.,
[`Sys.getlocale`](https://rdrr.io/r/base/locales.html)`("LC_TIME")`.

`quarters` returns a character vector of `"Q1"` to `"Q4"`.

`julian` returns the number of days (possibly fractional) since the
origin, with the origin as a `"origin"` attribute. All time calculations
in R are done ignoring leap-seconds.

## Note

Other components such as the day of the month or the year are very easy
to compute: just use
[`as.POSIXlt`](https://rdrr.io/r/base/as.POSIXlt.html) and extract the
relevant component. Alternatively (especially if the components are
desired as character strings), use
[`strftime`](https://rdrr.io/r/base/strptime.html).

## See also

[`DateTimeClasses`](https://rdrr.io/r/base/DateTimeClasses.html),
[`Date`](https://rdrr.io/r/base/Dates.html);
[`Sys.getlocale`](https://rdrr.io/r/base/locales.html)`("LC_TIME")`
crucially for [`months()`](https://rdrr.io/r/base/weekday.POSIXt.html)
and [`weekdays()`](https://rdrr.io/r/base/weekday.POSIXt.html).

## Examples

``` r
## first two are locale dependent:
weekdays(.leap.seconds)
#>  [1] "Saturday"  "Monday"    "Tuesday"   "Wednesday" "Thursday"  "Saturday" 
#>  [7] "Sunday"    "Monday"    "Tuesday"   "Wednesday" "Thursday"  "Friday"   
#> [13] "Monday"    "Friday"    "Monday"    "Tuesday"   "Wednesday" "Thursday" 
#> [19] "Friday"    "Monday"    "Tuesday"   "Friday"    "Sunday"    "Thursday" 
#> [25] "Sunday"    "Wednesday" "Sunday"   
months  (.leap.seconds)
#>  [1] "July"    "January" "January" "January" "January" "January" "January"
#>  [8] "January" "January" "July"    "July"    "July"    "July"    "January"
#> [15] "January" "January" "July"    "July"    "July"    "January" "July"   
#> [22] "January" "January" "January" "July"    "July"    "January"
quarters(.leap.seconds)
#>  [1] "Q3" "Q1" "Q1" "Q1" "Q1" "Q1" "Q1" "Q1" "Q1" "Q3" "Q3" "Q3" "Q3" "Q1" "Q1"
#> [16] "Q1" "Q3" "Q3" "Q3" "Q1" "Q3" "Q1" "Q1" "Q1" "Q3" "Q3" "Q1"

## Show how easily you get month, day, year, day (of {month, week, yr}), ... :
## (remember to count from 0 (!): mon = 0..11, wday = 0..6,  etc !!)

##' Transform (Time-)Date vector  to  convenient data frame :
dt2df <- function(dt, dName = deparse(substitute(dt))) {
    DF <- as.data.frame(unclass(as.POSIXlt( dt )))
    `names<-`(cbind(dt, DF, deparse.level=0L), c(dName, names(DF)))
}
## e.g.,
dt2df(.leap.seconds)    # date+time
#>    .leap.seconds sec min hour mday mon year wday yday isdst zone gmtoff
#> 1     1972-07-01   0   0    0    1   6   72    6  182     0  GMT      0
#> 2     1973-01-01   0   0    0    1   0   73    1    0     0  GMT      0
#> 3     1974-01-01   0   0    0    1   0   74    2    0     0  GMT      0
#> 4     1975-01-01   0   0    0    1   0   75    3    0     0  GMT      0
#> 5     1976-01-01   0   0    0    1   0   76    4    0     0  GMT      0
#> 6     1977-01-01   0   0    0    1   0   77    6    0     0  GMT      0
#> 7     1978-01-01   0   0    0    1   0   78    0    0     0  GMT      0
#> 8     1979-01-01   0   0    0    1   0   79    1    0     0  GMT      0
#> 9     1980-01-01   0   0    0    1   0   80    2    0     0  GMT      0
#> 10    1981-07-01   0   0    0    1   6   81    3  181     0  GMT      0
#> 11    1982-07-01   0   0    0    1   6   82    4  181     0  GMT      0
#> 12    1983-07-01   0   0    0    1   6   83    5  181     0  GMT      0
#> 13    1985-07-01   0   0    0    1   6   85    1  181     0  GMT      0
#> 14    1988-01-01   0   0    0    1   0   88    5    0     0  GMT      0
#> 15    1990-01-01   0   0    0    1   0   90    1    0     0  GMT      0
#> 16    1991-01-01   0   0    0    1   0   91    2    0     0  GMT      0
#> 17    1992-07-01   0   0    0    1   6   92    3  182     0  GMT      0
#> 18    1993-07-01   0   0    0    1   6   93    4  181     0  GMT      0
#> 19    1994-07-01   0   0    0    1   6   94    5  181     0  GMT      0
#> 20    1996-01-01   0   0    0    1   0   96    1    0     0  GMT      0
#> 21    1997-07-01   0   0    0    1   6   97    2  181     0  GMT      0
#> 22    1999-01-01   0   0    0    1   0   99    5    0     0  GMT      0
#> 23    2006-01-01   0   0    0    1   0  106    0    0     0  GMT      0
#> 24    2009-01-01   0   0    0    1   0  109    4    0     0  GMT      0
#> 25    2012-07-01   0   0    0    1   6  112    0  182     0  GMT      0
#> 26    2015-07-01   0   0    0    1   6  115    3  181     0  GMT      0
#> 27    2017-01-01   0   0    0    1   0  117    0    0     0  GMT      0
dt2df(Sys.Date() + 0:9) # date
#>    Sys.Date() + 0:9 sec min hour mday mon year wday yday isdst zone gmtoff
#> 1        2025-12-11   0   0    0   11  11  125    4  344     0  UTC      0
#> 2        2025-12-12   0   0    0   12  11  125    5  345     0  UTC      0
#> 3        2025-12-13   0   0    0   13  11  125    6  346     0  UTC      0
#> 4        2025-12-14   0   0    0   14  11  125    0  347     0  UTC      0
#> 5        2025-12-15   0   0    0   15  11  125    1  348     0  UTC      0
#> 6        2025-12-16   0   0    0   16  11  125    2  349     0  UTC      0
#> 7        2025-12-17   0   0    0   17  11  125    3  350     0  UTC      0
#> 8        2025-12-18   0   0    0   18  11  125    4  351     0  UTC      0
#> 9        2025-12-19   0   0    0   19  11  125    5  352     0  UTC      0
#> 10       2025-12-20   0   0    0   20  11  125    6  353     0  UTC      0

##' Even simpler:  Date -> Matrix - dropping time info {sec,min,hour, isdst}
d2mat <- function(x) simplify2array(unclass(as.POSIXlt(x))[4:7])
## e.g.,
d2mat(seq(as.Date("2000-02-02"), by=1, length.out=30)) # has R 1.0.0's release date
#>       mday mon year wday
#>  [1,]    2   1  100    3
#>  [2,]    3   1  100    4
#>  [3,]    4   1  100    5
#>  [4,]    5   1  100    6
#>  [5,]    6   1  100    0
#>  [6,]    7   1  100    1
#>  [7,]    8   1  100    2
#>  [8,]    9   1  100    3
#>  [9,]   10   1  100    4
#> [10,]   11   1  100    5
#> [11,]   12   1  100    6
#> [12,]   13   1  100    0
#> [13,]   14   1  100    1
#> [14,]   15   1  100    2
#> [15,]   16   1  100    3
#> [16,]   17   1  100    4
#> [17,]   18   1  100    5
#> [18,]   19   1  100    6
#> [19,]   20   1  100    0
#> [20,]   21   1  100    1
#> [21,]   22   1  100    2
#> [22,]   23   1  100    3
#> [23,]   24   1  100    4
#> [24,]   25   1  100    5
#> [25,]   26   1  100    6
#> [26,]   27   1  100    0
#> [27,]   28   1  100    1
#> [28,]   29   1  100    2
#> [29,]    1   2  100    3
#> [30,]    2   2  100    4

# \donttest{
## Julian Day Number (JDN, https://en.wikipedia.org/wiki/Julian_day)
## is the number of days since noon UTC on the first day of 4317 BCE.
## in the proleptic Julian calendar.  To more recently, in
## 'Terrestrial Time' which differs from UTC by a few seconds
## See https://en.wikipedia.org/wiki/Terrestrial_Time
julian(Sys.Date(), -2440588) # from a day
#> [1] 2461021
#> attr(,"origin")
#> [1] -2440588
floor(as.numeric(julian(Sys.time())) + 2440587.5) # from a date-time
#> [1] 2461021
# }
```
