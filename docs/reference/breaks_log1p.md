# A scales breaks generator for log1p scales

A scales breaks generator for log1p scales

## Usage

``` r
breaks_log1p(n = 5, base = 10)
```

## Arguments

- n:

  the number of breaks

- base:

  the base for the breaks

## Value

a function for ggplot scale breaks

## Examples

``` r
ggplot2::ggplot(ggplot2::diamonds, ggplot2::aes(x=price))+
  ggplot2::geom_density()+
  ggplot2::scale_x_continuous(trans="log1p", breaks=breaks_log1p())
```
