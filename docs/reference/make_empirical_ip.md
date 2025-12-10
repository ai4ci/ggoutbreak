# Recover a long format infectivity profile from an `EpiEstim` style matrix

Recover a long format infectivity profile from an `EpiEstim` style
matrix

## Usage

``` r
make_empirical_ip(omega, normalise = TRUE)
```

## Arguments

- omega:

  a matrix of probabilities, starting at time zero, with columns
  representing one possible infectivity profile, with the fist value
  being the probability at time zero (to 0.5). Alternatively this can be
  a vector of probabilities for one single profile, resulting in 1
  bootstrap.

- normalise:

  is this a probability mass function? In which case we make the sum of
  this equal to one (the default). If `FALSE` then the input matrix, or
  vector is clipped so that its maximum value is one. If this is a
  number then the PMF is scaled to this value.

## Value

a long format `ip` delay distribution

## Examples

``` r
format_ip(make_empirical_ip(c(0,0,1,1,1,2,2,2,1,1)))
#> [1] "mean: 5.64; sd: 2.03"
```
