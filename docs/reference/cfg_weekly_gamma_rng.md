# Weekly delay function with day of week effect

This function returns a random number generator from a gamma
distribution that has a weekly period and configurable degree of
additional variability around weekly pattern. It is suited to delays of
things like testing which may depend on the day of week.

## Usage

``` r
cfg_weekly_gamma_rng(
  mean = c(1, 1, 1, 1, 4, 3, 2),
  sd = sqrt(mean),
  week_starts = weekdays(as.Date("2024-10-14"))
)
```

## Arguments

- mean:

  a mean amount of delay for each day of the week

- sd:

  the SD of the delay

- week_starts:

  locale description of first day of week (default is a "Monday").

## Value

a random number generator taking `t` time parameter and returning a
duration the that time `t` depending on the day of the week.

## Examples

``` r
fn = cfg_weekly_gamma_rng(c(1,1,1,1,4,3,2))
matrix(fn(1:42),ncol=7,byrow=TRUE)
#>            [,1]      [,2]      [,3]      [,4]      [,5]      [,6]       [,7]
#> [1,] 1.08641963 6.8155467 1.9760900 8.0821977 0.7375058 1.3381693 0.08969051
#> [2,] 0.17190983 0.9741694 1.3303021 0.7651640 4.1862424 0.4389224 0.43008356
#> [3,] 0.06361773 6.3169688 0.7769338 0.4975902 1.3216847 0.8802695 0.18040673
#> [4,] 0.53048240 3.6359255 2.9786923 1.9336943 1.1536887 0.3358088 0.53352742
#> [5,] 0.78839729 0.5108545 4.0479716 0.1951924 0.3334846 0.7118283 1.83446885
#> [6,] 0.68615325 3.2840124 2.1744995 1.9013039 1.1313860 1.4806989 0.24784653
```
