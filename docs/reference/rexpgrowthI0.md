# Randomly sample incident times in an exponentially growing process with initial case load

Randomly sample incident times in an exponentially growing process with
initial case load

## Usage

``` r
rexpgrowthI0(I0, r, t_end, t_start = 0)
```

## Arguments

- I0:

  the expected number of cases observed in the first day

- r:

  and exponential growth rate (per unit time)

- t_end:

  the end of the observation period

- t_start:

  the start of the observation period

## Value

a vector of `n` samples from an exponential growth process

## Examples

``` r
graphics::hist(rexpgrowthI0(10,0.1,20), breaks=40)

graphics::hist(rexpgrowthI0(1000,-0.1,40), breaks=40)
```
