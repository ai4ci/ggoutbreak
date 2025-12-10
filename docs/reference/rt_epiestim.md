# `EpiEstim` reproduction number wrapper function

Calculate a reproduction number estimate from incidence data using the
`EpiEstim` library and an empirical generation time distribution. This
uses resampling to transmit uncertainty in generation time estimates.
This is quite slow for each time series depending on the number of
bootstraps and samples in the infectivity profile.

## Usage

``` r
rt_epiestim(
  df = i_incidence_input,
  ip = i_discrete_ip,
  bootstraps = 2000,
  window = 14,
  mean_prior = 1,
  std_prior = 2,
  ...,
  .progress = interactive()
)
```

## Arguments

- df:

  Count data. Extra groups are allowed. - a dataframe with columns:

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

- ip:

  infectivity profile - a dataframe with columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - tau (integer + complete) - the days since the index event.

  Minimally grouped by: boot (and other groupings allowed).

- bootstraps:

  - the number of bootstraps to take to calculate for each point.

- window:

  - the width of the `EpiEstim` window

- mean_prior:

  the prior for the \$R_t\$ estimate. When sample size is low the
  \$R_t\$ estimate will revert to this prior. In `EpiEstim` the default
  is a high number to allow detection of insufficient data but this
  tends to create anomalies in the early part of infection time series.
  A possible value is \$R_0\$ but in fact this also will be a poor
  choice for the value of \$R_t\$ when case numbers drop to a low value.

- std_prior:

  the prior for the \$R_t\$ SD.

- ...:

  not used

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

This will calculate a reproduction number for each group in the input
dataframe.

## Examples

``` r
data = example_poisson_rt_smooth()

tmp2 = data %>%
   rt_epiestim(ip=example_ip())

if (interactive()) {
  plot_rt(tmp2, date_labels="%b %y")+sim_geom_function(data,colour="red")
}
```
