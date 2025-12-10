# SEIR model with time-varying transmission parameter

This function simulates an SEIR
(Susceptible-Exposed-Infectious-Recovered) model where the transmission
rate (`beta`) varies over time.

## Usage

``` r
sim_seir_model(
  changes = dplyr::tibble(t = c(0, 30), dBeta = c(1, 0.5)),
  mean_latent_period,
  mean_gen_time,
  R0 = 2.5,
  fn_dBeta = cfg_step_fn(changes),
  N = 10000,
  imports = 10,
  max_time = 104,
  seed = Sys.time()
)
```

## Arguments

- changes:

  a dataframe holding change time points (`t`) and the proportional
  increase in the transmission rate in the `dBeta` column

- mean_latent_period:

  Mean time from infection to becoming infectious (E to I), assumed to
  be exponentially distributed.

- mean_gen_time:

  The average generation time (will be latent period+infectious
  duration).

- R0:

  the initial reproduction number

- fn_dBeta:

  A function of time `t` that returns the multiple of transmission rate
  at that time point. Transmission (beta) at time zero is defined by R0
  and the infectious duration, This is multiplied at each time by this
  factor.

- N:

  Total population size. Defaults to 10,000.

- imports:

  Initial number of infectious individuals. Defaults to 10.

- max_time:

  Maximum simulation time (in days or time units). Defaults to 100.

- seed:

  a random seed

## Value

A dataframe containing the following columns:

- statistic (character) - An identifier for the statistic, whether that
  be infections, admissions, deaths

- count (positive_integer) - Positive case counts associated with the
  specified time frame

- time (ggoutbreak::time_period + group_unique) - A (usually complete)
  set of singular observations per unit time as a `time_period`

Minimally grouped by: statistic (and other groupings allowed).

## Details

The latent period (time from infection to becoming infectious) is
assumed to be exponentially distributed. The infectious period is
derived from the given mean of the generation time and the latent
period.

The model assumes:

- Latent period ~ Exponential(sigma), where sigma = 1 /
  `mean_latent_period`

- Infectious period ~ Exponential(gamma), where gamma = 1 /
  (mean_gen_time - mean_latent_period)

- Generation time ~ Exponential(sigma) + Exponential(gamma)

- beta0 = R0 \* gamma

- Transmission rate beta(t) = fn_dBeta(t) \* beta0

The generation time is defined as the sum of the latent and infectious
periods.

## Examples

``` r
# Example: Lockdown after day 30

seir_output <- sim_seir_model(
  mean_latent_period = 4,
  mean_gen_time = 7,
  R0 = 2.5,
  fn_dBeta = function(t) ifelse(t < 30, 1, 0.5)
)

if (interactive()) {
  plot_ip(attr(seir_output,"ip"))
  plot_counts(seir_output)
}
```
