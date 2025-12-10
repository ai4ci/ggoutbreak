# Aggregate time series data preserving the time series

This function operates on timeseries data not linelists (see
[`time_summarise()`](https://ai4ci.github.io/ggoutbreak/reference/time_summarise.md))
for line lists. If a very granular timeseries is regrouped and this
function is applied the resulting dataframe will be

## Usage

``` r
time_aggregate(
  df = i_timestamped,
  ...,
  .groups = NULL,
  .cols = NULL,
  .fns = NULL
)
```

## Arguments

- df:

  an optionally grouped time series. Grouping should not include the
  time column. The grouping works differently from
  [`dplyr::summarise`](https://dplyr.tidyverse.org/reference/summarise.html)
  in that the last level of non-time groups is lost in this operation,
  so the subgroup you wish to aggregate should be included in the
  grouping.

- ...:

  A set of
  [`dplyr::summarise`](https://dplyr.tidyverse.org/reference/summarise.html)
  statements, or additional parameters for `.fns`

- .groups:

  as per
  [`dplyr::summarise`](https://dplyr.tidyverse.org/reference/summarise.html)

- .cols:

  Optional dplyr column specification for the data that will be
  summarised. This is useful if there are lots of columns you want to
  summarise using the same function (e.g. `sum` usually). `.cols` is
  passed to a
  [`dplyr::across`](https://dplyr.tidyverse.org/reference/across.html)
  call. if `.fns` is given and the `.cols` parameter is not specified
  then the columns to summarise are automatically identified. In doing
  this any `Date` columns are dropped. If this in not what you want then
  `.cols` or `...` must be given

- .fns:

  Optional a set of function specifications as per
  [`dplyr::across`](https://dplyr.tidyverse.org/reference/across.html)

## Value

the summarised time series preserving the `time` column, and with the
grouping structure involving one fewer levels than the input

## Examples

``` r
example_england_covid_by_age() %>%
  time_aggregate(count = sum(count), denom = sum(denom)) %>%
  dplyr::glimpse()
#> Rows: 1,410
#> Columns: 3
#> $ time  <t[day]> 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 4…
#> $ count <dbl> 2, 2, 13, 3, 19, 0, 1, 0, 0, 5, 1, 1, 4, 2, 1, 1, 1, 1, 2, 0, 0,…
#> $ denom <dbl> 38, 38, 247, 57, 361, 0, 19, 0, 0, 95, 19, 19, 76, 38, 19, 19, 1…

example_england_covid_by_age() %>%
  time_aggregate(.fns=mean, .cols=dplyr::where(is.numeric)) %>%
  dplyr::glimpse()
#> Rows: 1,410
#> Columns: 4
#> $ time       <t[day]> 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, …
#> $ count      <dbl> 0.10526316, 0.10526316, 0.68421053, 0.15789474, 1.00000000,…
#> $ denom      <dbl> 2, 2, 13, 3, 19, 0, 1, 0, 0, 5, 1, 1, 4, 2, 1, 1, 1, 1, 2, …
#> $ population <dbl> 2962472, 2962472, 2962472, 2962472, 2962472, 2962472, 29624…
```
