# logit scale

Perform logit scaling with correct axis formatting. To not be used
directly but with ggplot (e.g. ggplot2::scale_y_continuous(trans =
"logit") )

## Usage

``` r
logit_trans(n = 5, ...)
```

## Arguments

- n:

  the number of breaks

- ...:

  not used, for compatibility

## Value

A scales object

## Examples

``` r
dplyr::tibble(pvalue = c(0.001, 0.05, 0.1), fold_change = 1:3) %>%
 ggplot2::ggplot(ggplot2::aes(fold_change , pvalue)) +
 ggplot2::geom_point() +
 ggplot2::scale_y_continuous(trans = "logit")
```
