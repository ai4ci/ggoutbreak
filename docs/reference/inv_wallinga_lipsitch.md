# Calculate a growth rate from a reproduction number and an infectivity profile,

This solves the relationship between \$R_t\$ and growth rates as
described by Wallinga and Lipsitch, to get a growth rate from \$R_t\$
and infectivity profile.

## Usage

``` r
inv_wallinga_lipsitch(
  Rt,
  y = i_empirical_ip,
  a1 = seq(0.5, length.out = length(y)),
  a0 = dplyr::lag(a1, default = 0)
)
```

## Arguments

- Rt:

  a vector of reproduction numbers

- y:

  an empirical infectivity profile as a probability vector or as a
  dataframe of format: A dataframe containing the following columns:

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

an vector of growth rates

## Details

This function uses a single empirical distribution for the infectivity
profile / generation time. If multiple are provided then the average
central value is chosen (i.e. this does not propagate uncertainty in
infectivity profile)

## Examples

``` r
inv_wallinga_lipsitch(Rt=seq(0.5,2.5,length.out=9), y=example_ip())
#> [1] -0.13146405 -0.05656638  0.00000000  0.04589395  0.08478342  0.11865588
#> [7]  0.14875936  0.17591664  0.20070417
```
