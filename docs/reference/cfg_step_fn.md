# Step function from dataframe

Step function from dataframe

## Usage

``` r
cfg_step_fn(changes, ..., col_name = setdiff(colnames(changes), "t"))
```

## Arguments

- changes:

  a dataframe with `t` and `<col_name>` columns which define the cut
  points for a step function.

- ...:

  not used

- col_name:

  the value column (optional if only 2 columns)

## Value

a function that inputs a vector `t` and returns the next smallest
corresponding value in `<col_name>` (or the first one)
