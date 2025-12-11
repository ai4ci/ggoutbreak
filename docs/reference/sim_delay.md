# Apply a time-varying probability and delay function to linelist data

Apply a time-varying probability and delay function to linelist data

## Usage

``` r
sim_delay(
  df = i_sim_linelist,
  p_fn,
  delay_fn,
  input = "time",
  output = "event",
  seed = Sys.time()
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

- p_fn:

  Function that returns a probability between 0 and 1 for each row of
  the input dataframe. A `purrr` style lambda is OK (e.g. `~ 1` for
  always true) the first parameter of this will be time of infection.
  The function must be vectorised on its inputs (and consume additional
  inputs with `...`)

- delay_fn:

  A function that calculates the time to event onset from the `input`
  time. This will be called with a vector of infection times as the
  first parameter (`time`) but all other columns of `df` are also
  available as well as the `symptomatic`,`died`,and `admitted` flags.
  The function must be vectorised on its inputs (and consume additional
  inputs with `...`). A `purrr` style lambda is OK e.g.
  `~ stats::rgamma(.x, shape = 3)`, and the first parameter will be
  infection time. if you have an discrete probability profile for this
  then you can use `cfg_ip_sampler_rng(ip_symptoms)` without the tilde.

- input:

  a time column from which to calculate the delay from.

- output:

  an output column set name (defaults to `"event"`)

- seed:

  RNG seed for reproducibility

## Value

the line list with extra columns with prefix given by `output`,
specifying whether the event was observed, the delay and the simulation
time.

## Examples

``` r
tmp = sim_branching_process(
  changes = dplyr::tibble(t = c(0,20,40,60,80,110), R = c(1.8,1.5,0.9,1.5,0.8,1.2)),
  max_time = 120,
  seed = 100
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
#> .
#> .
#> .
#> complete

tmp2 = tmp %>% sim_delay(
  p_fn = ~ rbern(.x, 0.8),
  delay_fn = ~ rgamma2(.x, mean = 5),
)
tmp2 %>% dplyr::glimpse()
#> Rows: 35,205
#> Columns: 8
#> $ time                <t[day]> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ id                  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,…
#> $ generation_interval <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ infector            <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ generation          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ event               <lgl> TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, …
#> $ event_delay         <dbl> 2.906903, 8.446592, 6.541336, NA, 8.186022, 2.4223…
#> $ event_time          <t[day]> 2.91, 8.45, 6.54, NA, 8.19, 2.42, 4.4, NA, 2.41…
```
