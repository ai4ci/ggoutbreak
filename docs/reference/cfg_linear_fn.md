# Linear function from dataframe

Linear function from dataframe

## Usage

``` r
cfg_linear_fn(changes, ..., col_name = setdiff(colnames(changes), "t"))
```

## Arguments

- changes:

  a dataframe with `t` and `<col_name>` columns which define the change
  points for a piecewise linear function.

- ...:

  not used

- col_name:

  the value column (optional if only 2 columns)

## Value

a function that inputs a vector `t` and returns a linearly interpolated
value from `<col_name>`
