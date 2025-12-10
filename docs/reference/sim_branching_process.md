# Generate a line list from a branching process model parametrised by reproduction number

Generate a line list from a branching process model parametrised by
reproduction number

## Usage

``` r
sim_branching_process(
  changes = dplyr::tibble(t = c(0, 40), rt = c(2, 0.8)),
  max_time = 80,
  seed = Sys.time(),
  fn_Rt = cfg_step_fn(changes),
  fn_ip = ~example_ip(),
  fn_kappa = ~1,
  imports_df = NULL,
  fn_imports = ~ifelse(.x == 0, 30, 0),
  fn_list_next_gen = list(),
  max_size = 10000,
  ...
)
```

## Arguments

- changes:

  a dataframe containing a `t` time column and `R` reproduction number
  parameter. This parameter is optional if `fn_Rt` is specified

- max_time:

  maximum duration of simulation

- seed:

  random seed

- fn_Rt:

  can be specified instead of `changes` df. This is a vectorised
  function that accepts a time parameter and returns a reproduction
  number. If both this and `changes` are specified this takes
  preference.

- fn_ip:

  a function that takes input vector `t` (and/or `class`) and returns an
  infectivity profile at times `t`.

- fn_kappa:

  a vectorised function taking `t` and other imported case metadata
  returning a dispersion parameter controlling the likelihood of
  individual super-spreading. This must be between 1 and `Inf` with 1
  being standard poisson dispersion and larger values representing over
  dispersion.

- imports_df:

  a data frame containing minimally `time` and `count` columns plus any
  metadata about the imports in additional columns. Metadata columns can
  inform the `fn_Rt`,`fn_kappa` and `fn_ip` functions as additional
  parameters.

- fn_imports:

  a time varying function the defines the number of infected
  importations. If `imports_df` is defined then this is used instead

- fn_list_next_gen:

  a named list of functions. The name corresponds to metadata columns in
  the simulation, the function is a `purrr` style mapping that will
  replace the old value in the named column with a new one. Such a
  function can be generated with
  [`cfg_transition_fn()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_transition_fn.md)
  when a transition probability matrix is involved, of it can be
  specified directly as a `case_when` style function. The function must
  be vectorised and assume no grouping structure. If the function has
  named parameters it can reference any of the metadata columns, or time
  (as `t`). The
  [`rcategorical()`](https://ai4ci.github.io/ggoutbreak/reference/rcategorical.md)
  function may be useful in this scenario.

- ...:

  not used

## Value

a line list of cases for a simulated outbreak

A dataframe containing the following columns:

- id (unique_id) - Patient level unique id

- time (ggoutbreak::time_period) - Time of infection. A \`time_period\`

Any grouping possible.

## Examples

``` r
tmp = sim_branching_process(
  changes = dplyr::tibble(t = c(0,40), R = c(1.5,0.8)),
  max_time = 120,
  seed = 100,
  fn_imports = ~ ifelse(.x<10,1,0)
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
#> complete

if(interactive()) {
  plot_cases(tmp, mapping=ggplot2::aes(fill=as.factor(generation)),linewidth=0.1, colour="white")
}

# imports can also be specified as a dataframe, which allows additional
# metadata in the line list. An example of which is as follows:
imports_df = dplyr::tribble(
  ~time, ~variant, ~count,
  0:4, "wild-type", 100,
  10:14, "alpha", 5,
)
```
