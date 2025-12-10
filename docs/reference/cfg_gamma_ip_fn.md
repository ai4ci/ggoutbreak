# Get a IP generating function from time varying mean and SD of a gamma function

Get a IP generating function from time varying mean and SD of a gamma
function

## Usage

``` r
cfg_gamma_ip_fn(mean_fn = ~2, sd_fn = function(mean) sqrt(mean))
```

## Arguments

- mean_fn:

  a function which gives the time-varying mean of a gamma distribution,
  The function will be called minimally with `.x` or `t` which will be
  the time as a time period. Other variables may be present.

- sd_fn:

  a function which gives the time-varying mean of a gamma distribution.
  The function will be called with minimally `.x` or `t` which will be
  the time period and `.y` or `mean` will be the mean. Other variables
  may be present.

## Value

a time dependent function that inputs a time (as a time_period) and
returns an ip delay distribution for each day defined by the `mean_fn`
and `sd_fn`

## Examples

``` r
fn = cfg_gamma_ip_fn(mean_fn = \(t) ifelse(t < 5, 4, 2))
# a gamma function that changes mean at time 5
fn(4)
#> [[1]]
#> # A tibble: 15 × 5
#> # Groups:   boot [1]
#>      tau    a0    a1 probability  boot
#>    <int> <dbl> <dbl>       <dbl> <int>
#>  1     0   0     0.5    0.00175      1
#>  2     1   0.5   1.5    0.0639       1
#>  3     2   1.5   2.5    0.177        1
#>  4     3   2.5   3.5    0.221        1
#>  5     4   3.5   4.5    0.194        1
#>  6     5   4.5   5.5    0.141        1
#>  7     6   5.5   6.5    0.0898       1
#>  8     7   6.5   7.5    0.0527       1
#>  9     8   7.5   8.5    0.0290       1
#> 10     9   8.5   9.5    0.0152       1
#> 11    10   9.5  10.5    0.00771      1
#> 12    11  10.5  11.5    0.00378      1
#> 13    12  11.5  12.5    0.00181      1
#> 14    13  12.5  13.5    0.000848     1
#> 15    14  13.5  14.5    0.000707     1
#> 
fn(7)
#> [[1]]
#> # A tibble: 11 × 5
#> # Groups:   boot [1]
#>      tau    a0    a1 probability  boot
#>    <int> <dbl> <dbl>       <dbl> <int>
#>  1     0   0     0.5    0.0902       1
#>  2     1   0.5   1.5    0.352        1
#>  3     2   1.5   2.5    0.271        1
#>  4     3   2.5   3.5    0.151        1
#>  5     4   3.5   4.5    0.0748       1
#>  6     5   4.5   5.5    0.0345       1
#>  7     6   5.5   6.5    0.0153       1
#>  8     7   6.5   7.5    0.00657      1
#>  9     8   7.5   8.5    0.00277      1
#> 10     9   8.5   9.5    0.00115      1
#> 11    10   9.5  10.5    0.000786     1
#> 
```
