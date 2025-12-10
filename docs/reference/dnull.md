# Null distributions always returns NA

Null distributions always returns NA

## Usage

``` r
dnull(x, ...)
```

## Arguments

- x:

  vector of quantiles

- ...:

  not used

## Examples

``` r
dnull(c(0.25,0.5,0.75), 0.5, 0.25)
#> [1] NA NA NA
```
