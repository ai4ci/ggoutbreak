# Coerce an object to a `ggoutbreak` compatible time series dataframe

Coerce an object to a `ggoutbreak` compatible time series dataframe

## Usage

``` r
timeseries(x, ...)

# Default S3 method
timeseries(x, ...)

# S3 method for class 'incidence2'
timeseries(x, ...)

# S3 method for class 'data.frame'
timeseries(
  x,
  ...,
  date = NULL,
  count = NULL,
  denom = NULL,
  population = NULL,
  class = NULL,
  declutter = FALSE
)

# S3 method for class 'incidence'
timeseries(x, ..., declutter = FALSE)
```

## Arguments

- x:

  An object to coerce e.g. a data frame

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

- date:

  R expression defining a date column (as a `Date` - optional)

- count:

  R expression defining a count column (as an `int` - optional)

- denom:

  R expression defining a denominator column (as an `int` - optional)

- population:

  R expression defining a population column (as an `numeric` - optional)

- class:

  R expression defining a class column (as an `factor` - optional)

- declutter:

  logical flag. If `TRUE` then unnecessary original columns will be
  dropped

## Value

minimally a case count dataframe A dataframe containing the following
columns:

- count (positive_integer) - Positive case counts associated with the
  specified time frame

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

Any grouping allowed.

## Examples

``` r
data("mers_2014_15", package="EpiEstim")

set_default_unit("1 day")
#> [1] "7d 0H 0M 0S"

# Use an expression to generate a case count:
# N.B. complex column names need to be surrounded with bcakticks like this:

tmp = mers_2014_15$incidence %>%
  timeseries(
    date = `mers$dates`,
    count = local+imported
  ) %>%
  dplyr::glimpse()
#> Rows: 495
#> Columns: 5
#> $ time         <t[day]> -3796, -3795, -3794, -3793, -3792, -3791, -3790, -3789…
#> $ `mers$dates` <date> 2014-08-11, 2014-08-12, 2014-08-13, 2014-08-14, 2014-08-…
#> $ local        <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, …
#> $ imported     <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ count        <dbl> 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, …

if (interactive()) {
  ip = make_fixed_ip(mean = mers_2014_15$si$mean_si, sd = mers_2014_15$si$std_si)
  tmp %>% poisson_locfit_model(ip=ip,window=14) %>% plot_rt()
}


# remove columns not needed by `ggoutbreak`
outbreaks::dengue_fais_2011 %>%
  timeseries(date = onset_date, count = value, declutter = TRUE) %>%
  dplyr::glimpse()
#> Rows: 57
#> Columns: 2
#> $ time  <t[day]> -4857, -4850, -4843, -4836, -4829, -4822, -4815, -4808, -4801…
#> $ count <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…


# date column already exists,  could use `onset` or `death` as count
outbreaks::ebola_kikwit_1995 %>%
  timeseries(count = onset) %>%
  dplyr::glimpse()
#> Rows: 192
#> Columns: 6
#> $ time      <t[day]> -1.095e+04, -1.095e+04, -1.095e+04, -1.095e+04, -1.095e+0…
#> $ date      <date> 1995-01-06, 1995-01-07, 1995-01-08, 1995-01-09, 1995-01-10,…
#> $ onset     <int> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ death     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ reporting <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ count     <int> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …

# This data set needs grouping to make it a unique timeseries:
ukc19::ltla_cases %>%
  timeseries(anchor="start", unit="1 day") %>%
  dplyr::glimpse()
#> inferring extra groups to make `time` unique: code, name
#> Rows: 512,050
#> Columns: 7
#> Groups: code, name [381]
#> $ time       <t[day]> 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, …
#> $ code       <chr> "S12000033", "S12000033", "S12000033", "S12000033", "S12000…
#> $ date       <date> 2020-03-04, 2020-03-05, 2020-03-06, 2020-03-07, 2020-03-08…
#> $ name       <chr> "Aberdeen City", "Aberdeen City", "Aberdeen City", "Aberdee…
#> $ codeType   <chr> "LAD19", "LAD19", "LAD19", "LAD19", "LAD19", "LAD19", "LAD1…
#> $ count      <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 2, 1, 0, 1, 0, 0, 3,…
#> $ population <dbl> 228670, 228670, 228670, 228670, 228670, 228670, 228670, 228…

# from an incidence object:
if (requireNamespace("incidence",quietly = TRUE)) {

  onset = outbreaks::ebola_sim$linelist$date_of_onset
  sex = outbreaks::ebola_sim$linelist$gender
  inc.week.gender = incidence::incidence(onset, interval = 7, groups = sex, standard = FALSE)

  inc.week.gender %>% timeseries() %>% dplyr::glimpse()

  d = Sys.Date() + sample(-3:10, 10, replace = TRUE)
  di = incidence::incidence(d, interval = "week", first_date = Sys.Date() - 10, standard = TRUE)
  di %>% timeseries(declutter=TRUE) %>% dplyr::glimpse()
}
#> Rows: 112
#> Columns: 4
#> Groups: class [2]
#> $ dates <date> 2014-04-07, 2014-04-07, 2014-04-14, 2014-04-14, 2014-04-21, 201…
#> $ class <fct> f, m, f, m, f, m, f, m, f, m, f, m, f, m, f, m, f, m, f, m, f, m…
#> $ count <int> 1, 0, 0, 1, 4, 1, 4, 0, 9, 3, 8, 10, 8, 7, 9, 11, 13, 10, 7, 15,…
#> $ time  <t[week]> -560.3, -560.3, -559.3, -559.3, -558.3, -558.3, -557.3, -557…
#> Rows: 4
#> Columns: 2
#> $ time  <t[week]> 46.71, 47.71, 48.71, 49.71
#> $ count <int> 0, 2, 6, 2

if (requireNamespace("incidence2",quietly = TRUE)) {
  dat = outbreaks::ebola_sim_clean$linelist
  x = incidence2::incidence(dat, "date_of_onset", groups = c("gender", "hospital"), interval="week")
  x %>% timeseries() %>% dplyr::glimpse()
}
#> Rows: 601
#> Columns: 5
#> Groups: statistic, gender, hospital [12]
#> $ gender    <fct> f, m, f, f, m, f, f, f, f, f, f, f, f, m, m, m, f, f, f, f, …
#> $ hospital  <fct> Military Hospital, Connaught Hospital, NA, other, other, NA,…
#> $ time      <t[week]> 2310, 2311, 2312, 2312, 2312, 2313, 2313, 2313, 2313, 23…
#> $ statistic <fct> date_of_onset, date_of_onset, date_of_onset, date_of_onset, …
#> $ count     <int> 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 5, 1, 1, 1, 1, 3, 1, 2, 1, …
```
