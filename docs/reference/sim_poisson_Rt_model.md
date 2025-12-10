# Generate an outbreak case count series defined by Reproduction number using a poisson model.

Generate an outbreak case count series defined by Reproduction number
using a poisson model.

## Usage

``` r
sim_poisson_Rt_model(
  changes = dplyr::tibble(t = c(0, 40), rt = c(2.5, 0.8)),
  kappa = 1,
  max_time = 80,
  seed = Sys.time(),
  fn_Rt = cfg_step_fn(changes),
  fn_imports = ~ifelse(.x == 0, 30, 0),
  fn_ip = ~example_ip(),
  time_unit = "1 day"
)
```

## Arguments

- changes:

  a dataframe holding change time points (`t`) and reproduction number
  (`rt`) columns

- kappa:

  a dispersion parameter. 1 is no dispersion (compared to poisson),
  smaller values mean more dispersion.

- max_time:

  the desired length of the time series,

- seed:

  a random seed

- fn_Rt:

  a function that takes input vector `t` and returns the instantaneous
  reproduction number at time `t`

- fn_imports:

  a function that takes input vector `t` and returns the number of
  imported cases at times `t`.

- fn_ip:

  a function that takes input vector `t` and returns an infectivity
  profile at times `t`.

- time_unit:

  e.g. a daily or weekly time series: "1 day", "7 days"

## Value

A dataframe of case counts

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
tmp = sim_poisson_Rt_model(kappa=1, seed=100, fn_imports = ~ ifelse(.x %in% c(0,50),100,0))

if (interactive()) {
  ggplot2::ggplot(tmp,ggplot2::aes(x=time,y=count))+ggplot2::geom_point()+
    ggplot2::geom_line()
}
```
