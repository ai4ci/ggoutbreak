# Summarise a line list

This function converts a line list into a daily count of incident cases,
plus infections, admissions, deaths, test samples, test results if
present. Censoring of these counts can also be defined. Whilst
summarising various network measures such as the forward looking case
reproduction number are also calculated.

## Usage

``` r
sim_summarise_linelist(
  df = i_sim_linelist,
  ...,
  censoring = list(admitted = function(t) rgamma2(t, mean = 5), death = function(t)
    rgamma2(t, mean = 10), sample = function(t, result_delay) result_delay),
  max_time = max(df$time)
)
```

## Arguments

- df:

  a line list dataframe arising from e.g.
  [`sim_branching_process()`](https://ai4ci.github.io/ggoutbreak/reference/sim_branching_process.md) -
  a dataframe with columns:

  - id (unique_id) - Patient level unique id

  - time (ggoutbreak::time_period) - Time of infection. A
    \`time_period\`

  Any grouping allowed.

- ...:

  the grouping to include in the summarisation.

- censoring:

  a named list of column names (without the `_time` suffix) of the kind
  created by
  [`sim_delay()`](https://ai4ci.github.io/ggoutbreak/reference/sim_delay.md)
  or
  [`sim_apply_delay()`](https://ai4ci.github.io/ggoutbreak/reference/sim_apply_delay.md),
  and an associated function defining the delay of reporting that the
  column experiences. For this function `t` (or `.x` for a purrr lambda)
  will refer to the `XX_time` column, i.e. whenever the event that is
  being reported happened and `time` the simulation infection time. N.B.
  since infection is not observed you can't censor it.

- max_time:

  the censoring time for this observation. If this is a vector there
  will be multiple time series in the output

## Value

a count data frame with additional statistics.

A dataframe containing the following columns:

- statistic (character) - An identifier for the statistic, whether that
  be infections, admissions, deaths

- count (positive_integer) - Positive case counts associated with the
  specified time frame

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a \`time_period\`

Minimally grouped by: statistic (and other groupings may be present).

## Examples

``` r
sim = sim_branching_process(
  changes = dplyr::tibble(t = c(0,40), R = c(1.7,0.8)),
  max_time = 120,
  seed = 100,
  fn_imports = ~ ifelse(.x==0,100,0)
)
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> complete

tmp = sim %>% sim_summarise_linelist()

p1 = plot_counts(tmp)

p2 = ggplot2::ggplot(tmp, ggplot2::aes(x=as.Date(time)))+
  ggplot2::geom_point(ggplot2::aes(y=rt.case,colour="case"))+
  ggplot2::geom_point(ggplot2::aes(y=rt.inst,colour="instantaneous"))+
  ggplot2::geom_line(ggplot2::aes(y=rt.weighted))+
  ggplot2::coord_cartesian(ylim=c(0,3.5))+
  ggplot2::xlab(NULL)

patchwork::wrap_plots(p1,p2,ncol=1,axes="collect")

```
