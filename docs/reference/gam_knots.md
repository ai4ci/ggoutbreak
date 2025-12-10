# Derive a set of knot points for a GAM from data

At the moment this does nothing sophisticated. An mostly equally spaced
grid of knots with gaps at start and end to prevent over-fitting there.
In the future this could look at the number of observations or areas
where there is a lot of change to add in more knots.

## Usage

``` r
gam_knots(data = i_incidence_data, window, ..., k = NULL)
```

## Arguments

- data:

  the function will be called with incidence data - a dataframe with
  columns:

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Any grouping allowed.

- window:

  the spacing between knots

- ...:

  currently not used

- k:

  alternative to `window`, if `k` is given then the behaviour of the
  knots will be similar to the default `mgcv::s(...,k=...)` parameter.

## Value

a vector of times (as a numeric)

## Examples

``` r
gam_knots(example_poisson_rt(), 14)
#> [1] 10 20 30 40 50 60 70 75
gam_knots(example_poisson_rt(), k=10)
#>  [1]  0.000000  8.888889 17.777778 26.666667 35.555556 44.444444 53.333333
#>  [8] 62.222222 71.111111 80.000000
```
