# Null distributions always returns NA

Null distributions always returns NA

## Usage

``` r
qnull(p, ...)
```

## Arguments

- p:

  vector of probabilities

- ...:

  not used

## Examples

``` r
qnull(c(0.25,0.5,0.75), 0.5, 0.25)
#> [1] NA NA NA
```
