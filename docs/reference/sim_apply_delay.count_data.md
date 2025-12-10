# Apply delay distributions to count data

This function uses convolution of time delay functions (which can
themselves be a function of time, e.g. weekly periodicity) with incident
infections to generate realistic looking outbreak metrics, including
admission, death, symptom onset and testing.

## Usage

``` r
sim_apply_delay.count_data(
  df,
  ...,
  fn_p_symptomatic = ~0.5,
  fn_symptom_profile = cfg_gamma_ip_fn(~5),
  fn_p_admitted = ~0.1,
  fn_admission_profile = cfg_weekly_ip_fn(c(8, 8, 8, 8, 8, 9.5, 9)),
  fn_p_died = ~0.05,
  fn_death_profile = cfg_gamma_ip_fn(~14),
  fn_p_tested = ~0.8,
  fn_sample_profile = cfg_weekly_ip_fn(c(1, 1, 1, 1, 1, 1.5, 1.4)),
  fn_result_profile = cfg_weekly_ip_fn(c(1, 1, 1, 1, 1, 1.6, 1.5)),
  seed = Sys.time()
)
```

## Arguments

- df:

  the output of
  [`sim_poisson_model()`](https://ai4ci.github.io/ggoutbreak/reference/sim_poisson_model.md)
  or
  [`sim_summarise_linelist()`](https://ai4ci.github.io/ggoutbreak/reference/sim_summarise_linelist.md),
  including a `count` column and a `time` column

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

  Named arguments passed on to `sim_apply_delay.count_data`

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

- fn_symptom_profile, fn_admission_profile, fn_death_profile:

  a function that takes time and returns the probability density of
  symptoms, admissions, or deaths over time since infection (i.e. `tau`)
  as an ip delay distribution. If possible it is a very good idea to
  pre-compute these distributions as they need to be assigned to every
  line in the input and this can be very slow.

- fn_sample_profile:

  a function that takes time and returns the probability density of test
  sample being taken over time since symptoms.

- fn_result_profile:

  a function that takes time and returns the probability density of test
  result being available over time since test sampling.

- seed:

  RNG seed for reproducibility

## Value

a long format set of counts of infections, symptom, admitted, death,
sample (tests taken), results (test results).

## Examples

``` r
tmp = sim_poisson_model(seed=100) %>% sim_apply_delay()

if(interactive()) {
  plot_counts(tmp, mapping=ggplot2::aes(colour=statistic))+
    ggplot2::geom_line()
}
```
