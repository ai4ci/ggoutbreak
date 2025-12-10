# A random Bernoulli sample as a logical value

A random Bernoulli sample as a logical value

## Usage

``` r
rbern(n, prob)
```

## Arguments

- n:

  number of observations

- prob:

  the mean probability (vectorised)

## Value

a vector of logical values of size `n`

## Examples

``` r
table(rbern(100, 0.25))
#> 
#> FALSE  TRUE 
#>    73    27 
```
