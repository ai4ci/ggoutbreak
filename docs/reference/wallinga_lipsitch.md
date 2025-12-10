# Calculate the reproduction number from a growth rate estimate and an infectivity profile

This function uses a single empirical distribution for the infectivity
profile / generation time. If multiple are provided then the average
central value is chosen (i.e. this does not propagate uncertainty in
infectivity profile)

## Usage

``` r
wallinga_lipsitch(
  r,
  y = i_empirical_ip,
  a1 = seq(0.5, length.out = length(y)),
  a0 = dplyr::lag(a1, default = 0)
)
```

## Arguments

- r:

  a growth rate (may be a vector)

- y:

  an empirical infectivity profile either as a probability vector or as
  a dataframe of format: A dataframe containing the following columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - a0 (double) - the beginning of the time period (in days)

  - a1 (double) - the end of the time period (in days)

  Minimally grouped by: boot (and other groupings allowed).

- a1:

  the end time of the infectivity profile probability estimate (defaults
  to 0.5,1.5,2.5,...).

- a0:

  the start time of the infectivity profile probability estimate
  (defaults to 0,0.5,1.5,...).

## Value

a reproduction number estimate based on `r`

## Examples

``` r
# using a probability vector.
wallinga_lipsitch(r=seq(-0.1,0.1,length.out=9), y=stats::dgamma(1:50, 5,2))
#> [1] 0.8524904 0.8882735 0.9247920 0.9620371 1.0000000 1.0386712 1.0780412
#> [8] 1.1181002 1.1588382

# using an infectivity profile
wallinga_lipsitch(r=seq(-0.1,0.1,length.out=9), y=example_ip())
#> [1] 0.5949656 0.6805519 0.7759966 0.8821707 1.0000000 1.1304667 1.2746113
#> [8] 1.4335343 1.6083980
```
