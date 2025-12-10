# Apply a ascertainment bias to the observed case counts.

Apply a ascertainment bias to the observed case counts.

## Usage

``` r
sim_apply_ascertainment(df = i_sim_count_data, fn_asc = ~1, seed = Sys.time())
```

## Arguments

- df:

  a count dataframe from e.g.
  [`sim_poisson_model()`](https://ai4ci.github.io/ggoutbreak/reference/sim_poisson_model.md)
  or
  [`sim_summarise_linelist()`](https://ai4ci.github.io/ggoutbreak/reference/sim_summarise_linelist.md) -
  a dataframe with columns:

  - statistic (character) - An identifier for the statistic, whether
    that be infections, admissions, deaths

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Minimally grouped by: statistic (and other groupings allowed).

- fn_asc:

  a function that takes a single input vector `t` and returns a
  probability of ascertainment, e.g. `~ stats::rbeta(.x, 20, 80)` or
  `~ rbeta2(.x,prob=<probability>,kappa=<dispersion>)`. or
  [`cfg_weekly_proportion_rng()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_weekly_proportion_rng.md)

- seed:

  a RNG seed

## Value

a dataframe with `original` column, and `count` column modified to
include ascertainment bias.

## Examples

``` r
with_defaults("2025-01-01" ,"1 day", {
  dplyr::tibble(
    statistic = "incidence",
    time=as.time_period(1:10,"1 day"),
    count=rep(100,10)
  ) %>%
  dplyr::group_by(statistic) %>%
  sim_apply_ascertainment(~ ifelse(.x<=5,0.1,0.9))
})
```
