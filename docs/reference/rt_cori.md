# Reproduction number estimate using the Cori method

Calculate a reproduction number estimate from incidence data using a
reimplementation of the Cori method and an empirical generation time
distribution. This uses a mixture distribution to transmit uncertainty
in generation time estimates. A number of changes compared to the
original `EpiEstim` implementation have been made. Firstly there is no
technical limitation to the infectivity profile being strictly positive
in time. This allows use of serial intervals and secondary potentially
delayed observations. Secondly this implementation should tolerate
missing count values (NA values must be filtered out though). Thirdly
for a given time point `t` this applies all `Rt` estimates for which the
window spans time point `t` rather than end on time point `t`, which
tends to address lag issues with the original, and fourthly this
implementation allows multiple window widths to be calculated in
parallel and aggregated. All of this tends to increase uncertainty in
the result particularly in the time dimension, which addresses some of
the issue seem with `EpiEstim` during the pandemic. Finally it is quite
a bit quicker, especially if approximate quantiles are all that are
needed.

## Usage

``` r
rt_cori(
  df = i_incidence_input,
  ip = i_discrete_ip,
  window = 14,
  mean_prior = 1,
  std_prior = 2,
  ...,
  epiestim_compat = FALSE,
  approx = FALSE,
  .progress = interactive()
)
```

## Arguments

- df:

  The count data. Extra groups are allowed. - a dataframe with columns:

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

- ip:

  A long format infectivity profile. - a dataframe with columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - tau (integer + complete) - the days since the index event.

  Minimally grouped by: boot (and other groupings allowed).

- window:

  - the widths of the Cori method window to include in the estimate.
    This can be a vector of values and all windows will be calculated
    and aggregated.

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

- epiestim_compat:

  produce an estimate of `Rt` using only windows that end on the time
  `t` rather than all windows that span time `t`. If this option is
  selected there can also only be one value for window.

- approx:

  approximate the quantiles of the mixture distribution with a gamma
  distribution with the same first mean and SD.

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

There are still issues with large \$R_t\$ estimates in the early part of
a time series, which is a resul tof the renewal equaltion method.

This will calculate a reproduction number for each group in the input
dataframe.

## Examples

``` r
data = example_poisson_rt_smooth()

tmp2 = data %>% rt_cori(ip=example_ip(), epiestim_compat = TRUE)
tmp3 = data %>% rt_cori(ip=example_ip(), window=c(5:14), approx=TRUE)

comp = dplyr::bind_rows(
  tmp2 %>% dplyr::mutate(class = "EpiEstim"),
  tmp3 %>% dplyr::mutate(class = "Cori+")
) %>% dplyr::group_by(class)

if (interactive()) {
  plot_rt(comp, date_labels="%b %y")+sim_geom_function(data,colour="black")+
    ggplot2::coord_cartesian(ylim=c(0.5,3.0))
}
```
