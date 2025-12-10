# Make an infectivity profile from posterior samples

The infectivity profile is typically fitted to data by MCMC as a gamma
distribution. This function generates a discrete infectivity probability
distribution representing the chance that an infectee was infected on
any specific day after the infector was infected (given that the
infectee was infected), from posterior samples.

## Usage

``` r
make_posterior_ip(
  ...,
  mean,
  sd,
  shape,
  rate,
  scale,
  epiestim_compat = FALSE,
  n_boots = 100
)
```

## Arguments

- ...:

  not used, must be empty

- mean:

  a vector of gamma distribution means

- sd:

  a vector of gamma distribution sds

- shape:

  a vector of gamma distribution shape parameters

- rate:

  a vector of gamma distribution rate parameters

- scale:

  a vector of gamma distribution scale parameters

- epiestim_compat:

  Use `EpiEstim` to generate the infectivity profiles. A true value here
  results in an infectivity profile with probability of 0 for day 0.

- n_boots:

  if there are more posterior samples than this limit then a maximum of
  `n_boots` ip distributions will be created (randomly sampled).

## Value

a long format `ip` delay distribution

## Details

If using `EpiEstim` and
[`coarseDataTools::dic.fit.mcmc`](http://nickreich.github.io/coarseDataTools/reference/dic.fit.mcmc.md)
the output of the MCMC will be a S4 object with a `samples` slot,
containing a dataframe of `shape=var1` and `scale=var2` columns. to use
this output with `make_posterior_ip` invoke it like this:

`do.call(make_posterior_ip, SI_fit_clever@samples %>% dplyr::rename(shape=var1, scale=var2))`

N.b. only one combination of mean and sd, shape and rate, or shape and
scale, are required.

## Examples

``` r
tmp = make_posterior_ip(
  mean = stats::rnorm(100,5,0.1),
  sd = stats::rnorm(100,1.5,0.1)
)
tmp %>% dplyr::glimpse()
#> Rows: 1,400
#> Columns: 5
#> Groups: boot [100]
#> $ tau         <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 0, 1, 2, 3, …
#> $ a0          <dbl> 0.0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.…
#> $ a1          <dbl> 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11…
#> $ probability <dbl> 4.834025e-09, 3.592446e-04, 2.028943e-02, 1.283010e-01, 2.…
#> $ boot        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2…
if (interactive()) plot_ip(tmp)
```
