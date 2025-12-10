# Apply a time varying probability and convolution to count data

Standard convolution assumes one delay distribution. This is actually
not what we see in reality as delays can depend on any factors,
including the day of week. This function applies a convolution to an
input time-series when that convolution is expressed as a function
(usually of time, but can be of anything in the input dataframe). The
convolution is then sampled using a poisson or negative binomial

## Usage

``` r
sim_convolution(
  df = i_sim_count_data,
  p_fn,
  delay_fn,
  ...,
  input = "infections",
  output,
  kappa = 1,
  from = c("count", "rate")
)
```

## Arguments

- df:

  a count dataframe from e.g.
  [`sim_poisson_model()`](https://ai4ci.github.io/ggoutbreak/reference/sim_poisson_model.md)
  or
  [`sim_summarise_linelist()`](https://ai4ci.github.io/ggoutbreak/reference/sim_summarise_linelist.md) -
  a dataframe with columns:

  - statistic (character) - An identifier for the statistic, whether
    that be infections, admissions, deaths

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Minimally grouped by: statistic (and other groupings allowed).

- p_fn:

  a function that takes a time parameter and potentially and returns
  probability of observation given something occurs. a no-op for this
  parameter is `~ 1`.

- delay_fn:

  a function that takes time and returns the probability of observation
  (given it occurred) over time since infection (i.e. `tau`) as an ip
  delay distribution. This does not have to sum to 1 (e.g. mapping
  incidence to prevalence) but if not then the combination of `p_fn` and
  `delay_fn` is less easy to interpret. This should behave sensibly if p
  changes halfway through a convolution. See
  [`cfg_weekly_ip_fn()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_weekly_ip_fn.md)
  and
  [`cfg_gamma_ip_fn()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_gamma_ip_fn.md)
  for helper functions to construct this parameter. A no-op for this
  parameter would be `~ ifelse(.x==0,1,0)`.

- ...:

  not used

- input:

  the input statistic

- output:

  the output statistic

- kappa:

  dispersion. scaled such that poisson dispersion is 1. Values must be 0
  (no dispersion), 1 (poisson dispersion) or greater than 1 for
  over-dispersion.

- from:

  Controls if you base future counts on previous counts or on underlying
  rate, defaults to `count` but `rate` is a possibility if you want to
  base the convolution off a more theoretical value rather than observed
  cases. Either way the convolution generates a new rate which is in
  turn sampled into a poisson or negative binomial count.

## Value

return the result of applying this convolution to the data.

## Examples

``` r
weekday_delay = make_fixed_ip(mean = 5, sd = 2)
weekend_delay = make_fixed_ip(mean = 6, sd = 2)

delay_fn = ~ ifelse(.x %% 7 %in% c(6,7), list(weekend_delay), list(weekday_delay))
p_fn = ~ ifelse(.x < 20, 0.5, 0.75)

data = dplyr::tibble(
    time=1:40,
    count = rep(100,40),
    rate = rep(100,40),
    statistic="infections") %>% dplyr::group_by(statistic)
delayed = data %>%
    sim_convolution(p_fn,delay_fn,output="delayed") %>%
    dplyr::filter(statistic=="delayed")
if (interactive()) ggplot2::ggplot(delayed,ggplot2::aes(x=time))+
  ggplot2::geom_line(ggplot2::aes(y=rate))+
  ggplot2::geom_line(ggplot2::aes(y=count))

# other example delay functions
delay_fn = cfg_gamma_ip_fn( ~ ifelse(.x<5, 8, 4))
```
