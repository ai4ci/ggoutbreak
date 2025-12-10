# Weekly convolution distribution function

Weekly convolution distribution function

## Usage

``` r
cfg_weekly_ip_fn(
  mean = c(1, 1, 1, 1, 4, 3, 2),
  sd = sqrt(mean),
  week_starts = weekdays(as.Date("2024-10-14"))
)
```

## Arguments

- mean:

  The means of a gamma distributed delay function for each weekday

- sd:

  The sds of a gamma distributed delay function for each weekday

- week_starts:

  locale description of first day of week (default is a "Monday").

## Value

a time dependent function that inputs a time (as a time_period) and
generates an IP delay distribution for each day varying by day of week

## Examples

``` r
cat(sapply(cfg_weekly_ip_fn()(1:7),format_ip),sep = "\n")
#> mean: 1.06; sd: 1.01
#> mean: 4; sd: 2.04
#> mean: 3; sd: 1.77
#> mean: 2.02; sd: 1.44
#> mean: 1.06; sd: 1.01
#> mean: 1.06; sd: 1.01
#> mean: 1.06; sd: 1.01
```
