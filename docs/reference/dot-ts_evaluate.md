# Evaluate a function in a timeseries dataframe

N.B. only exported as used in one vignette for demo purposes

## Usage

``` r
.ts_evaluate(fn, df)
```

## Arguments

- fn:

  a function.

- df:

  a dataframe with a numeric time column, plus other columns

## Value

a vector, the result of applying the function to df.

## Examples

``` r
test = dplyr::tibble(
  time = 1:10,
  class = rep(c("one","two"),5)
)
.ts_evaluate(function(t,class) {print(t); print(class); 1}, test )
#>  [1]  1  2  3  4  5  6  7  8  9 10
#>  [1] "one" "two" "one" "two" "one" "two" "one" "two" "one" "two"
#>  [1] 1 1 1 1 1 1 1 1 1 1
```
