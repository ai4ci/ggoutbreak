# A once only evaluation function generator

This function can be used in a package to create a package local
variable that is calculated once on first use and stored for the rest of
the session. If the expression throws an uncaught error the value is not
stored. The expression is evaluated in the environment it is first
called in.

## Usage

``` r
.singleton(
  expr,
  on_error = function(e) {
     stop("Initialising value failed:", e, call. = FALSE)
 }
)
```

## Arguments

- expr:

  an expression the value of which is to be cached.

- on_error:

  a function that takes a single value (the error) and handles or
  rethrows it.

## Value

the result of expr.

## Unit tests


    fixed_rnorm = .singleton(rnorm(10))
    a = fixed_rnorm()
    b = fixed_rnorm()
    testthat::expect_equal(a,b)
