# Wedge distribution

The wedge distribution has a support of 0 to 1 and has a linear
probability density function over that support.

## Usage

``` r
dwedge(x, a, lower.tail = TRUE, log = FALSE)
```

## Arguments

- x:

  vector of quantiles

- a:

  a gradient from -2 (left skewed) to 2 (right skewed)

- lower.tail:

  logical; if TRUE (default), probabilities are `P[X<=x]` otherwise
  `P[X>x]`.

- log:

  logical; if TRUE, probabilities p are given as log(p).

## Value

a vector of probabilities, quantiles, densities or samples.

## Details

The `rwedge` can be combined with quantile functions to skew standard
distributions, or introduce correlation or down weight certain parts of
the distribution.

## Examples

``` r
pwedge(seq(0,1,0.1), a=1)
#>  [1] 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5
dwedge(seq(0,1,0.1), a=1)
#>  [1] 0.000 0.055 0.120 0.195 0.280 0.375 0.480 0.595 0.720 0.855 1.000
qwedge(c(0.25,0.5,0.75), a=-1)
#> [1] 0.1771243 0.3819660 0.6339746

stats::cor(
  stats::qnorm(rwedge(1000, a=2)),
  stats::qnorm(rwedge(1000, a=-2))
)
#> [1] -0.007274514
```
