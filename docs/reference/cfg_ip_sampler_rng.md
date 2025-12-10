# Randomly sample from an empirical distribution

This is used for random sampling from the infectivity profile for times
to infection, for example. There is nothing to stop you putting in a
delay distribution with negative times but strange things may happen in
your simulation.

## Usage

``` r
cfg_ip_sampler_rng(ip = i_empirical_ip)
```

## Arguments

- ip:

  a long format empirical distribution - a dataframe with columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - a0 (double) - the beginning of the time period (in days)

  - a1 (double) - the end of the time period (in days)

  Minimally grouped by: boot (and other groupings allowed).

## Value

a function which accepts `n` parameter which produces random samples
from the `ip` distribution

## Examples

``` r
tmp = cfg_ip_sampler_rng(example_ganyani_ip())(10000)

# This discretised ganyani distribution is based on these figures:
# mean: 5.2 (3.78-6.78) and sd: 1.72 (0.91-3.93)
format_ip(example_ganyani_ip())
#> [1] "PDF: mean: 5.12 [3.58 — 6.77]; sd: 1.99 [0.849 — 3.16]; 100 bootstraps"

mean(tmp) # Should be about 5.2
#> [1] 5.202316
stats::sd(tmp) # Should be about 1.72
#> [1] 2.222033
```
