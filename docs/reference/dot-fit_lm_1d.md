# Fit a weighted 1D linear model and predict output

If all the weights are NA they are ignored.

## Usage

``` r
.fit_lm_1d(y, x)
```

## Arguments

- y:

  the y values. At least 2.

- x:

  the x values. At least 2.

## Value

a vector with intercept (`c`) and gradient (`m`)

## Unit tests


    testthat::expect_equal(.fit_lm_1d(y = 1:10 * 2 + 5, x = 1:10), c(c = 5, m = 2))
