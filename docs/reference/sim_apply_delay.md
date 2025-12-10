# Apply delay distribution to count or linelist data

Events include symptom onset, admission, death, test sampling, test
processing

## Usage

``` r
sim_apply_delay(
  df,
  ...,
  fn_p_symptomatic = ~0.5,
  fn_p_admitted = ~0.1,
  fn_p_died = ~0.05,
  fn_p_tested = ~0.8,
  seed = Sys.time()
)
```

## Arguments

- df:

  a line list dataframe arising from e.g.
  [`sim_branching_process()`](https://ai4ci.github.io/ggoutbreak/reference/sim_branching_process.md) -
  EITHER: a dataframe with columns:

  - id (unique_id) - Patient level unique id

  - time (ggoutbreak::time_period) - Time of infection. A
    \`time_period\`

  Any grouping allowed.

  OR with columns:

  - statistic (character) - An identifier for the statistic, whether
    that be infections, admissions, deaths

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Minimally grouped by: statistic (and other groupings allowed).

- ...:

  Named arguments passed on to
  [`sim_apply_delay.linelist`](https://ai4ci.github.io/ggoutbreak/reference/sim_apply_delay.linelist.md)

  `fn_symptom_delay,fn_admission_delay,fn_death_delay`

  :   a function that calculates the time to event onset from infection.
      This will be called with a vector of infection times as the first
      parameter (`time`) but all other columns of `df` are also
      available as well as the `symptomatic`,`died`,and `admitted`
      flags. The function must be vectorised on its inputs (and consume
      additional inputs with `...`). A `purrr` style lambda is OK e.g.
      `~ stats::rgamma(.x, shape = 3)`, and the first parameter will be
      infection time. if you have an discrete probability profile for
      this then you can use `cfg_ip_sampler_rng(ip_symptoms)`.

  `fn_sample_delay`

  :   This function returns the time from either symptom onset
      (symptomatic) or from infection (asymptomatic) until a sample is
      taken. (N.B. this might be better to do a screening test
      probability plus screening test frequency rather than overloading
      this.)

  `fn_result_delay`

  :   Identical to other functions except the first parameter will be
      `sample_time` rather than time of infection. This is the time from
      sampling to the result being available.

  Named arguments passed on to
  [`sim_apply_delay.count_data`](https://ai4ci.github.io/ggoutbreak/reference/sim_apply_delay.count_data.md)

  `fn_symptom_profile,fn_admission_profile,fn_death_profile`

  :   a function that takes time and returns the probability density of
      symptoms, admissions, or deaths over time since infection (i.e.
      `tau`) as an ip delay distribution. If possible it is a very good
      idea to pre-compute these distributions as they need to be
      assigned to every line in the input and this can be very slow.

  `fn_sample_profile`

  :   a function that takes time and returns the probability density of
      test sample being taken over time since symptoms.

  `fn_result_profile`

  :   a function that takes time and returns the probability density of
      test result being available over time since test sampling.

- fn_p_symptomatic, fn_p_admitted, fn_p_died, fn_p_tested:

  Function that returns a probability between 0 and 1 for each row of
  the input dataframe. A `purrr` style lambda is OK (e.g. `~ 1` for
  always true) the first parameter of this will be time of infection.
  The function must be vectorised on its inputs (and consume additional
  inputs with `...`)

- seed:

  RNG seed for reproducibility

## Value

Depends on input, either:

- a wide format line list with additional `XX`, `XX_time` and `XX_delay`
  columns, for each of the set of statistics generated.

- a long format set of counts of different statistics i.e. `infections`,
  `symptoms`, `admission`, `death`, `sample` (tests taken), `results`
  (test results) .

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

tmp2 = tmp %>% sim_apply_delay()
tmp2 %>% dplyr::glimpse()
#> Rows: 35,205
#> Columns: 19
#> $ time                <t[day]> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ id                  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,…
#> $ generation_interval <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ infector            <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ generation          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ symptom             <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE…
#> $ symptom_delay       <dbl> 2.828098, NA, NA, NA, NA, 3.922012, 8.274450, 2.95…
#> $ symptom_time        <t[day]> 2.83, NA, NA, NA, NA, 3.92, 8.27, 2.96, NA, 3.3…
#> $ admitted            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
#> $ admitted_delay      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ admitted_time       <t[day]> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ death               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
#> $ death_delay         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ death_time          <t[day]> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ tested              <lgl> TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, …
#> $ sample_delay        <dbl> 0.07154627, 0.50653917, NA, NA, 1.34214166, 0.3205…
#> $ sample_time         <t[day]> 0.07, 0.51, NA, NA, 1.34, 0.32, 2.14, 0.09, 0.0…
#> $ result_delay        <dbl> 0.07154627, 0.50653917, NA, NA, 1.34214166, 0.3205…
#> $ result_time         <t[day]> 0.14, 1.01, NA, NA, 2.68, 0.64, 4.28, 0.19, 0.0…
```
