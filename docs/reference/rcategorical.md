# Sampling from the multinomial equivalent of the Bernoulli distribution

Sampling from the multinomial equivalent of the Bernoulli distribution

## Usage

``` r
rcategorical(n, prob, factor = FALSE)
```

## Arguments

- n:

  a sample size

- prob:

  a (optionally named) vector of probabilities that will be normalised
  to sum to 1

- factor:

  if not FALSE then factor levels will either be taken from the names of
  `prob` first, or if this is a character vector from this.

## Value

a vector of random class labels of length `n`. Labels come from names of
`prob` or from a character vector in `factor`.

## Examples

``` r
prob = c("one"=0.1,"two"=0.2,"seven"=0.7)
table(rcategorical(1000,prob))
#> 
#>   one seven   two 
#>    83   700   217 
rcategorical(10,prob,factor=TRUE)
#>  [1] one   one   one   seven seven one   seven seven seven two  
#> Levels: one two seven
rcategorical(10,rep(1,26),factor=letters)
#>  [1] o u f r f l w j i w
#> Levels: a b c d e f g h i j k l m n o p q r s t u v w x y z
```
