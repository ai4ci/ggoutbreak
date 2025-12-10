# Generate a random probability based on features of the simulation

Generate a random probability based on features of the simulation

## Usage

``` r
cfg_beta_prob_rng(probability_fn = ~0.8, kappa_fn = ~0.1)
```

## Arguments

- probability_fn:

  a function which gives the time-varying mean of a beta distribution,
  The function will be called minimally with `.x` or \`t“ which will be
  the time as a time period. Other variables may be present.

- kappa_fn:

  a function which gives the time-varying dispersion of a beta
  distribution. The function will be called with minimally `.x` or `t`
  which will be the time period and `.y` or `mean` will be the mean.
  Other variables may be present.

## Value

a time dependent function that inputs a time (as a time_period) and
returns an probability for each day defined by the `probability_fn` and
`kappa_fn`

## See also

[`rbeta2()`](https://ai4ci.github.io/ggoutbreak/reference/rbeta2.md)

## Examples

``` r
fn = cfg_beta_prob_rng(~ ifelse(.x<=5,0.1,0.9))
fn(1:10)
#>  [1] 0.09994303 0.10665743 0.11315549 0.08053910 0.09749180 0.90247668
#>  [7] 0.90286478 0.90559741 0.89325780 0.91719053
```
