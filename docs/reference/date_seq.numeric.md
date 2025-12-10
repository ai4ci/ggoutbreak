# Create the full sequence of values in a vector

This is useful if you want to fill in missing values that should have
been observed but weren't. For example, date_seq(c(1, 2, 4, 6), 1) will
return 1:6.

## Usage

``` r
# S3 method for class 'numeric'
date_seq(x, period = 1, tol = 1e-06, ...)
```

## Arguments

- x:

  a numeric or date vector

- period:

  Gap between each observation. The existing data will be checked to
  ensure that it is actually of this periodicity.

- tol:

  Numerical tolerance for checking periodicity.

- ...:

  for subtype methods

## Value

a vector of the same type as the input

## Examples

``` r
date_seq(c(1, 2, 4, 5, 10), 1)
#>  [1]  1  2  3  4  5  6  7  8  9 10
```
