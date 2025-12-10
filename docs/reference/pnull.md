# Null distributions always returns NA

Null distributions always returns NA

## Usage

``` r
pnull(q, ...)
```

## Arguments

- q:

  vector of quantiles

- ...:

  not used

## Examples

``` r
pnull(c(0.25,0.5,0.75), 0.5, 0.25)
#> [1] NA NA NA
```
