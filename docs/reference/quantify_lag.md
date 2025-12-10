# Identify estimate lags in a model

A specific parameter or set of parameters can be estimated by a
`pipeline`. This function applies the `pipeline` to a synthetic epidemic
with sawtooth incidence resulting from a stepped growth rate function.
The lag between synthetic input and estimate is assessed by minimising
the root mean square error of input and estimated based on different lag
offsets.

## Usage

``` r
quantify_lag(pipeline, ip = i_empirical_ip, lags = -10:30)
```

## Arguments

- pipeline:

  a function taking an input dataset and an infectivity profile as
  inputs and producing an estimate as output. This is the whole
  parametrised pipeline including any other inputs. This can be a
  `purrr` style function, in which case the `.x` variable is the input
  dataset and `.y` is the infectivity profile.

- ip:

  the infectivity profile. - a dataframe with columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - a0 (double) - the beginning of the time period (in days)

  - a1 (double) - the end of the time period (in days)

  Minimally grouped by: boot (and other groupings allowed).

- lags:

  a vector with the delays to test. Defaults to -10 to +30 days

## Value

a lag analysis dataframe containing the `estimate` type and the `lag` in
days that the estimate is behind the actual observation

## Examples

``` r
set_default_start("2025-01-01")
#> [1] "2020-01-01"
# lags from a locfit incidence model with Rt estimation.
# This model has no estimator lag:
pipeline = ~ .x %>% poisson_locfit_model() %>% rt_from_incidence(ip = .y)
quantify_lag(pipeline, ip = example_ip())
#> Rt from incidence: assuming independence and approximating quantiles. 
#> (N.B. this message will only be displayed once.)
#> # A tibble: 3 × 2
#> # Groups:   estimate [3]
#>   estimate    lag
#> * <chr>     <int>
#> 1 growth        0
#> 2 incidence     0
#> 3 rt            0

# lags from an epiestim Rt estimation
# this model's lags depend on the infectivity profile.
# In this case it is 8 days
pipeline2 =  ~ .x %>% rt_epiestim(ip = .y)
quantify_lag(pipeline2, ip=example_ip() )
#> # A tibble: 1 × 2
#> # Groups:   estimate [1]
#>   estimate   lag
#> * <chr>    <int>
#> 1 rt           8
```
