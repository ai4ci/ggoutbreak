# Augment a line list of infection with a set of events

Events include symptom onset, admission, death, test sampling, test
processing

## Usage

``` r
sim_apply_delay.linelist(
  df = i_sim_linelist,
  ...,
  fn_p_symptomatic = ~0.5,
  fn_symptom_delay = ~rgamma2(.x, mean = 5),
  fn_p_admitted = ~0.1,
  fn_admission_delay = cfg_weekly_gamma_rng(c(8, 8, 8, 8, 8, 9.5, 9)),
  fn_p_died = ~0.05,
  fn_death_delay = ~rgamma2(.x, mean = 14),
  fn_p_tested = ~0.8,
  fn_sample_delay = cfg_weekly_gamma_rng(c(1, 1, 1, 1, 1, 1.5, 1.4)),
  fn_result_delay = cfg_weekly_gamma_rng(c(1, 1, 1, 1, 1, 1.6, 1.5)),
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

- ...:

  Named arguments passed on to `sim_apply_delay.linelist`

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

- fn_symptom_delay, fn_admission_delay, fn_death_delay, :

  a function that calculates the time to event onset from infection.
  This will be called with a vector of infection times as the first
  parameter (`time`) but all other columns of `df` are also available as
  well as the `symptomatic`,`died`,and `admitted` flags. The function
  must be vectorised on its inputs (and consume additional inputs with
  `...`). A `purrr` style lambda is OK e.g.
  `~ stats::rgamma(.x, shape = 3)`, and the first parameter will be
  infection time. if you have an discrete probability profile for this
  then you can use `cfg_ip_sampler_rng(ip_symptoms)`.

- fn_sample_delay:

  This function returns the time from either symptom onset (symptomatic)
  or from infection (asymptomatic) until a sample is taken. (N.B. this
  might be better to do a screening test probability plus screening test
  frequency rather than overloading this.)

- fn_result_delay:

  Identical to other functions except the first parameter will be
  `sample_time` rather than time of infection. This is the time from
  sampling to the result being available.

- seed:

  RNG seed for reproducibility

## Value

a line list with additional time and delay columns.

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
#> $ symptom             <lgl> FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE…
#> $ symptom_delay       <dbl> NA, NA, NA, 4.9253245, NA, 8.0370335, 4.4965400, 0…
#> $ symptom_time        <t[day]> NA, NA, NA, 4.93, NA, 8.04, 4.5, 0.67, 5.1, NA,…
#> $ admitted            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
#> $ admitted_delay      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 8.3546…
#> $ admitted_time       <t[day]> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 8.3…
#> $ death               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
#> $ death_delay         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ death_time          <t[day]> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ tested              <lgl> FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
#> $ sample_delay        <dbl> NA, NA, 0.6481758, 4.1093855, 2.5986856, 0.1081633…
#> $ sample_time         <t[day]> NA, NA, 0.65, 4.11, 2.6, 0.11, 0.6, 3.21, 1.33,…
#> $ result_delay        <dbl> NA, NA, 0.6481758, 4.1093855, 2.5986856, 0.1081633…
#> $ result_time         <t[day]> NA, NA, 1.3, 8.22, 5.2, 0.22, 1.2, 6.42, 2.66, …
```
