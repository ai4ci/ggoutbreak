# Add grouping to ensure a column is groupwise unique

Add grouping to ensure a column is groupwise unique

## Usage

``` r
.infer_grouping(df, unique_col)
```

## Arguments

- df:

  a dataframe, maybe grouped

- unique_col:

  a column reference that should be groupwise unique

## Value

a regrouped dataframe in which `unique_col` is indeed unique

## Unit tests


    df = dplyr::tibble(
      original_grp = as.vector(sapply(LETTERS[1:4],rep,20)),
      to_find = as.vector(sapply(letters[1:10],rep,8)),
      to_make_unique = rep(1:10,8),
      distractor = runif(80)
    ) 
    unique_col = as.symbol("to_make_unique")
    tmp = .infer_grouping(df, to_make_unique)
    testthat::expect_equal(
      format(groups(tmp)),
      c("original_grp", "to_find")
    )
