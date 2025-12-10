# Reproduction number from renewal equation applied to modelled incidence using statistical re-sampling

Calculate a reproduction number estimate from modelled incidence
estimates, by statistical sampling from a log-normally distributed
incidence estimate, such as you get from a `I_t ~ Poisson(I_0 e^{rt})`
model using a log link function. This is combined with uncertain
infectivity profile specified as multiple discrete empirical
distributions, to calculate the range of possible values for \$R_t\$.

## Usage

``` r
rt_from_renewal(
  df = i_incidence_model,
  ip = i_discrete_ip,
  bootstraps = 1000,
  seed = Sys.time(),
  .progress = interactive()
)
```

## Arguments

- df:

  modelled incidence estimate - a dataframe with columns:

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

- ip:

  infectivity profile - a dataframe with columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - tau (integer + complete) - the days since the index event.

  Minimally grouped by: boot (and other groupings allowed).

- bootstraps:

  the number of samples to take at each time point. This will be rounded
  up to a whole multiple of the infectivity profile distribution length.

- seed:

  a random number seed for reproducibility

- .progress:

  show a CLI progress bar

## Value

A dataframe containing the following columns:

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

- rt.fit (double) - an estimate of the reproduction number

- rt.se.fit (positive_double) - the standard error of the reproduction
  number

- rt.0.025 (double) - lower confidence limit of the reproduction number

- rt.0.5 (double) - median estimate of the reproduction number

- rt.0.975 (double) - upper confidence limit of the reproduction number

Any grouping allowed.

## Details

This method is moderately slow and non deterministic by default.

## Examples

``` r
data = example_poisson_rt_smooth()

tmp2 = data %>%
   poisson_locfit_model() %>%
   rt_from_renewal(ip=example_ip())

if (interactive()) {
  plot_rt(tmp2, date_labels="%b %y")+sim_geom_function(data,colour="red")
}
```
