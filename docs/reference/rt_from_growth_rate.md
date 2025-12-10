# Wallinga-Lipsitch reproduction number from growth rates

Calculate a reproduction number estimate from growth rate using the
Wallinga and Lipsitch 2007 estimation using empirical generation time
distribution. This uses resampling to transmit uncertainty in growth
rate estimates. This also handles time-series that are not on a daily
cadence (although this is experimental). The reproduction number
estimate is neither a instantaneous (backward looking) nor case (forward
looking) reproduction number but somewhere between the two, as the
method looks at a flux of infection at a single point in time.

## Usage

``` r
rt_from_growth_rate(
  df = i_growth_rate,
  ip = i_empirical_ip,
  bootstraps = 1000,
  seed = Sys.time(),
  .progress = interactive()
)
```

## Arguments

- df:

  Growth rate estimates - a dataframe with columns:

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  - growth.fit (double) - an estimate of the growth rate

  - growth.se.fit (positive_double) - the standard error the growth rate

  - growth.0.025 (double) - lower confidence limit of the growth rate

  - growth.0.5 (double) - median estimate of the growth rate

  - growth.0.975 (double) - upper confidence limit of the growth rate

  Any grouping allowed.

- ip:

  Infectivity profile - a dataframe with columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - a0 (double) - the beginning of the time period (in days)

  - a1 (double) - the end of the time period (in days)

  Minimally grouped by: boot (and other groupings allowed).

- bootstraps:

  - the number of bootstraps to take to calculate for each point.

- seed:

  a random number generator seed

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

This method is quite slow compared to some others and the default is non
deterministic.

## Examples

``` r
data = example_poisson_rt_smooth()

tmp = data %>%
  poisson_locfit_model() %>%
  rt_from_growth_rate(ip=example_ip())

if (interactive()) {
  plot_rt(tmp, date_labels="%b %y")+sim_geom_function(data,colour="red")
}
```
