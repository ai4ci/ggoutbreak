# Multinomial time-series model.

Takes a list of times, classes and counts, e.g. a COGUK variant like
data set with time, (multinomial) class (e.g. variant) and count being
the count in that time period. Fits a quadratic B-spline on time to the
proportion of the data using
[`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html), with
approx one degree of freedom per class and per `window` units of the
time series.

## Usage

``` r
multinomial_nnet_model(
  d = i_multinomial_input,
  ...,
  window = 14,
  frequency = "1 day",
  predict = TRUE,
  .progress = interactive()
)
```

## Arguments

- d:

  A multi-class count input dataframe - a dataframe with columns:

  - class (factor) - A factor specifying the type of observation. This
    will be things like variant, or serotype, for a multinomial model.
    Any missing data points are ignored.

  - count (positive_integer) - Positive case counts associated with the
    specified time frame

  - time (ggoutbreak::time_period + group_unique) - A (usually complete)
    set of singular observations per unit time as a \`time_period\`

  Minimally grouped by: class (and other groupings allowed).

- ...:

  not used

- window:

  a number of data points between knots, smaller values result in less
  smoothing, large value in more.

- frequency:

  the temporal density of the output estimates.

- predict:

  result a prediction. If false we return the model.

- .progress:

  show a CLI progress bar

## Value

a new dataframe with `time` (as a time period), `class`, and
`proportion.0.5`, or a model object

## Details

Additional groupings are treated as distinct proportions models.

## Examples

``` r
data = example_poisson_rt_2class()
tmp = data %>% multinomial_nnet_model(window=14)
#> # weights:  28 (13 variable)
#> initial  value 44630.360662 
#> iter  10 value 39354.669896
#> iter  20 value 38533.463749
#> final  value 38533.460670 
#> converged

if (interactive()) {
  plot_multinomial(tmp, date_labels="%b %y")
}
```
