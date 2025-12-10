# Generate a simple time-series of cases based on a growth rate step function

This time-series has no statistical noise and is useful for testing
things. It has a fixed known value for infections and growth rate (fixed
at 0.05 and -0.05 per day), and a instantaneous reproduction number
which is based on the provided infectivity profile. A fixed denominator
gives a known proportion and a relative growth rate that is the same as
the growth rate.

## Usage

``` r
sim_test_data(ip = example_ip(), duration = 500, period = 50)
```

## Arguments

- ip:

  an infectivity profile. any uncertainty will be collapsed into the
  central distribution.

- duration:

  the total length of the time-series

- period:

  the duration of each positive or negative growth phase

## Value

a time series with `count`, `incidence`, `growth`, `rt`, `proportion`
and `relative.growth` columns

## Examples

``` r
sim_test_data() %>% dplyr::glimpse()
#> Rows: 501
#> Columns: 8
#> $ time            <t[day]> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1…
#> $ growth          <dbl> 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, …
#> $ incidence       <dbl> 100.0000, 105.1271, 110.5171, 116.1834, 122.1403, 128.…
#> $ rt              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ denom           <dbl> 12182, 12182, 12182, 12182, 12182, 12182, 12182, 12182…
#> $ proportion      <dbl> 0.008208500, 0.008629359, 0.009071795, 0.009536916, 0.…
#> $ relative.growth <dbl> 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, …
#> $ count           <dbl> 100, 105, 111, 116, 122, 128, 135, 142, 149, 157, 165,…
```
