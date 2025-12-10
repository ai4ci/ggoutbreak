# Infer the prevalence of disease from incidence estimates and population size.

**\[experimental\]**

Log-scaled incidence estimates are used to generate samples of
incidence. These are convolved with the duration of infection (as the
probability still infected) on any given day (as a set of discrete
distributions) and the result is evaluated as a fraction of population.
The result is fitted to a logit-normal (using moments) and quantiles
produced from samples. If test sensitivity is used instead of duration
of infection then the prevalence estimate will be the expected
proportion of test positives by screening test rather than a true
prevalence. Differences between this and the proportion of test
positives observed may be due to ascertainment bias or test sensitivity
misspecification.

## Usage

``` r
infer_prevalence(
  modelled = i_incidence_model,
  pop = i_population_data,
  ip = i_discrete_ip,
  bootstraps = 1000,
  seed = Sys.time(),
  ...
)
```

## Arguments

- modelled:

  Model output from processing the `raw` dataframe with something like
  `poission_locfit_model` - a dataframe with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - incidence.fit (double) - an estimate of the incidence rate on a log
    scale

  - incidence.se.fit (positive_double) - the standard error of the
    incidence rate estimate on a log scale

  - incidence.0.025 (positive_double) - lower confidence limit of the
    incidence rate (true scale)

  - incidence.0.5 (positive_double) - median estimate of the incidence
    rate (true scale)

  - incidence.0.975 (positive_double) - upper confidence limit of the
    incidence rate (true scale)

  Any grouping allowed.

- pop:

  The population data must be grouped in the same way as `modelled`. - a
  dataframe with columns:

  - population (positive_integer) - Size of population

  Any grouping allowed.

- ip:

  A discrete distribution representing the duration of infection (or
  probability of detection of infection). This will not sum to one. - a
  dataframe with columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - tau (integer + complete) - the days since the index event.

  Minimally grouped by: boot (and other groupings allowed).

- bootstraps:

  the number of samples to take at each time point. This will be rounded
  up to a whole multiple of the infection duration distribution length.

- seed:

  a random number seed for reproducibility

- ...:

  not used

## Value

the modelled input with additional proportion columns

## Examples

``` r
tmp = example_poisson_rt_smooth() %>%
  poisson_locfit_model(window=14) %>%
  infer_prevalence(
     pop = 10000,
     ip = example_ip()
  )

if(interactive()) {
  plot_prevalence(tmp)
}
```
