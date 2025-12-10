# Reband any discrete distribution

e.g. age banded population, or a discrete probability distribution e.g.
a serial interval distribution. This method fits a monotonically
increasing spline to the cumulative distribution (including the upper
and lower limits) and interpolating using that spline to the new cut
points.

## Usage

``` r
reband_discrete(
  x,
  y,
  xout,
  xlim = c(0, NA),
  ytotal = c(0, sum(y)),
  digits = 0,
  labelling = c("positive_integer", "inclusive", "exclusive"),
  sep = "-"
)
```

## Arguments

- x:

  a set of upper limits of bands, e.g. for age: 0-14;15-64;65-79;80+ is
  15,65,80,NA

- y:

  a set of quantities for each band e.g. population figures

- xout:

  a set of new upper limits

- xlim:

  Upper and lower limits for x. if the last band is e.g 80+ in the input
  and we want to know the 85+ band in the output some kind of maximum
  upper limit is needed to interpolate to.

- ytotal:

  upper and lower limits for y. If the interpolation values fall outside
  of x then the minimum and maximum limits of y are given by this. This
  would be `c(0,1)` for a probability distribution, for example.

- digits:

  if the `xout` value is continuous then how many significant figures to
  put in the labels

- labelling:

  are the `xout` values interpretable as an `inclusive` upper limit, or
  an `exclusive` upper limit, or as an upper limit of an
  \`positive_integer“ quantity

- sep:

  separator for names e.g. `18-24` or `18 to 24`

## Value

a re-banded set of discrete values, guaranteed to sum to the same as `y`

## Examples

``` r
england_demographics = ukc19::uk_population_2019_by_10yr_age %>%
  dplyr::filter(name=="England")

ul = stringr::str_extract(england_demographics$class, "_([0-9]+)",group = 1) %>%
  as.numeric()
# ul is currently inclusive so add 1:
ul = ul + 1

tmp = reband_discrete(
  ul, england_demographics$population,
  c(5,10,15,40,80), xlim=c(0,120))

tmp
#>      0-4      5-9    10-14    15-39    40-79      80+ 
#>  3443507  3394336  3194025 18081321 25336808  2836964 

sum(tmp)
#> [1] 56286961
sum(england_demographics$population)
#> [1] 56286961
```
