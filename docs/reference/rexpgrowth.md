# Randomly sample incident times in an exponentially growing process

Randomly sample incident times in an exponentially growing process

## Usage

``` r
rexpgrowth(n, r, t_end, t_start = 0)
```

## Arguments

- n:

  the number of items to sample

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
graphics::hist(rexpgrowth(1000,0.1,40), breaks=40)

graphics::hist(rexpgrowth(1000,-0.1,40), breaks=40)
```
