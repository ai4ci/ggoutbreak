# Estimate growth rate from modelled incidence

This assumes a modelled incidence estimate that is log-normal. The
exponential growth rate is the first derivative of the mu parameters of
this log-normal. On the link scale these are normally distributed. This
function assumes that the time series incidence estimates are
uncorrelated to estimate the error in the growth rate, which is a
conservative approach resulting in more uncertainty in growth rate than
might be possible through other methods. This is all based on
Savitsky-Golay filters applied to the normally distributed log-incidence
estimates.

## Usage

``` r
growth_rate_from_incidence(d = i_incidence_model, window = 7, deg = 2)
```

## Arguments

- d:

  a modelled incidence estimate - a dataframe with columns:

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

- window:

  the width of the Savitsky-Golay filter - must be odd

- deg:

  the polynomial degree of the filter

## Value

the timeseries with growth rate columns: A dataframe containing the
following columns:

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

- growth.fit (double) - an estimate of the growth rate

- growth.se.fit (positive_double) - the standard error the growth rate

- growth.0.025 (double) - lower confidence limit of the growth rate

- growth.0.5 (double) - median estimate of the growth rate

- growth.0.975 (double) - upper confidence limit of the growth rate

Any grouping allowed.

## Examples

``` r
data = example_poisson_growth_rate()
tmp2 = data %>%
  poisson_glm_model(window=7,deg=1) %>%
  growth_rate_from_incidence(window = 13, deg=1)

if(interactive()) {
  plot_incidence(tmp2,
      date_labels="%b %y")

  plot_growth_rate(
      tmp2,
      date_labels="%b %y"
    )+
    sim_geom_function(data,colour="red")
}
```
