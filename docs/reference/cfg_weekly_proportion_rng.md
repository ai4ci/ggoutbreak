# Random probability function with day of week effect

This function returns a random probability generator that has a weekly
period and configurable degree of additional variability around weekly
pattern. It is suited to probabilities of things like testing which may
depend on the day of week.

## Usage

``` r
cfg_weekly_proportion_rng(
  prob = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.5, 0.5),
  kappa = 0.1,
  week_starts = weekdays(as.Date("2024-10-14"))
)
```

## Arguments

- prob:

  the rates of e.g. ascertainment for each day of the week.

- kappa:

  dispersion parameter between 0 and 1. O is no dispersion. 1 is maximum

- week_starts:

  locale description of first day of week (default is a "Monday").

## Value

a random number generator function taking `t` time parameter and
returning a probability of ascertainment for that time, depending on day
of week etc.

## Examples

``` r
fn = cfg_weekly_proportion_rng(c(0.9,0.9,0.9,0.9,0.9,0.1,0.1))
matrix(fn(1:42),ncol=7,byrow=TRUE)
#>           [,1]      [,2]       [,3]       [,4]      [,5]      [,6]      [,7]
#> [1,] 0.9174375 0.9022554 0.09517817 0.09628920 0.8979259 0.8899529 0.8954396
#> [2,] 0.8828648 0.8942341 0.10573120 0.09786131 0.9071478 0.9139198 0.9071535
#> [3,] 0.8930483 0.8982471 0.09208758 0.09737471 0.9094128 0.8943528 0.8894383
#> [4,] 0.9101896 0.8971038 0.08945290 0.09997201 0.8997584 0.8991862 0.9045917
#> [5,] 0.9069867 0.8974974 0.09865388 0.09596240 0.9041086 0.8914366 0.9073698
#> [6,] 0.8907440 0.8831391 0.09738555 0.09781887 0.8899456 0.8962666 0.9039236
```
