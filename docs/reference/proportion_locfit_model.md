# A binomial proportion estimate and associated exponential growth rate

takes a list of times, counts and a denominator and fits a
quasi-binomial model using a logit link function to proportion data
using local regression using the package `locfit`.

## Usage

``` r
proportion_locfit_model(
  d = i_proportion_input,
  ...,
  window = 14,
  deg = 1,
  frequency = "1 day",
  predict = TRUE,
  .progress = interactive()
)
```

## Arguments

- d:

  the input - a dataframe with columns:

  - denom (positive_integer) - Total test counts associated with the
    specified time frame

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

- ...:

  not used and present to allow proportion model to be used in a
  `group_modify`

- window:

  a number of data points defining the bandwidth of the estimate,
  smaller values result in less smoothing, large value in more. The
  default value of 14 is calibrated for data provided on a daily
  frequency, with weekly data a lower value may be preferred. - default
  `14`

- deg:

  polynomial degree (min 1) - higher degree results in less smoothing,
  lower values result in more smoothing. A degree of 1 is fitting a
  linear model piece wise. - default `2`

- frequency:

  the density of the output estimates as a time period such as `7 days`
  or `2 weeks`. - default `1 day`

- predict:

  result is a prediction dataframe. If false we return the `locfit`
  models (advanced). - default `TRUE`

- .progress:

  show a CLI progress bar

## Value

A dataframe containing the following columns:

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

- proportion.fit (double) - an estimate of the proportion on a logit
  scale

- proportion.se.fit (positive_double) - the standard error of proportion
  estimate on a logit scale

- proportion.0.025 (proportion) - lower confidence limit of proportion
  (true scale)

- proportion.0.5 (proportion) - median estimate of proportion (true
  scale)

- proportion.0.975 (proportion) - upper confidence limit of proportion
  (true scale)

- relative.growth.fit (double) - an estimate of the relative growth rate

- relative.growth.se.fit (positive_double) - the standard error the
  relative growth rate

- relative.growth.0.025 (double) - lower confidence limit of the
  relative growth rate

- relative.growth.0.5 (double) - median estimate of the relative growth
  rate

- relative.growth.0.975 (double) - upper confidence limit of the
  relative growth rate

Any grouping allowed.

## Details

This expects d to contain one combination of:

- `time` and `count` and `denom` columns - e.g. all tests conducted.

This results is a one versus others comparison binomial proportion
estimate plus a relative growth rate estimate specifying how much
quicker this is growing compared to the growth of the denominator.

The denominator maybe the sum of all subgroups `denom = sum(count)`,
e.g. in the situation where there are multiple variants of a disease
circulating. In which case the relative growth is that of the subgroup
compared to the overall. You can make this a one-versus-others
comparison by making the denominator exclude the current item (e.g.
`denom = sum(count)-count`).

The denominator can also be used to express the size of the population
tested. This gives us a relative growth rate that is different in
essence to the previous and may be a better estimate of the true growth
rate in the situation where testing effort is variable, or capacity
saturated.

## Examples

``` r
data = example_poisson_rt_2class()

tmp2 = data %>% proportion_locfit_model(window=7,deg=2)
tmp3 = data %>% proportion_locfit_model(window=14,deg=1)

comp = dplyr::bind_rows(
  tmp2 %>% dplyr::mutate(model="7:2"),
  tmp3 %>% dplyr::mutate(model="14:1"),
) %>% dplyr::group_by(model,class)

if (interactive()) {
  plot_proportion(
    comp,
    date_labels="%b %y",
    mapping=ggplot2::aes(colour=model),
    raw=data
  )+ggplot2::facet_wrap(~class)
}
```
