# Generate a multinomial outbreak defined by per class growth rates and a poisson model

Generate a multinomial outbreak defined by per class growth rates and a
poisson model

## Usage

``` r
sim_multinomial(
  changes = dplyr::tibble(t = c(0, 20, 40, 60, 80), variant1 = c(0.1, 0, -0.1, 0, 0.1),
    variant2 = c(0.15, 0.05, -0.05, -0.01, 0.05), variant3 = c(0, 0.05, -0.05, +0.05,
    -0.05), ),
  initial = c(100, 100, 100),
  time_unit = "1 day",
  ...
)
```

## Arguments

- changes:

  a list of time points in column `t` and growth rates per week per
  class, in other columns.

- initial:

  the size of the initial outbreak per class. There should be one entry
  per class

- time_unit:

  e.g. a daily or weekly time series: "1 day", "7 days"

- ...:

  Named arguments passed on to
  [`sim_poisson_model`](https://ai4ci.github.io/ggoutbreak/reference/sim_poisson_model.md)

  `fn_imports`

  :   a function that takes input vector `t` and returns the number of
      imported cases at times `t`.

  `seed`

  :   a random seed

  `kappa`

  :   a dispersion parameter. 1 is no dispersion (compared to poisson),
      smaller values mean more dispersion.

  `max_time`

  :   the desired length of the time series,

  `fn_growth`

  :   a function that takes input vector `t` and returns the growth
      rates at times `t`

## Value

a case count time series including `class`, `count` and `time` columns

## Examples

``` r
if (interactive()) {
  plot_counts(
    sim_multinomial() %>% dplyr::glimpse()
  )
}
```
