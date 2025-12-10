# Sample from a multinomial transition matrix

This is particularly designed for use within the `fn_list_next_gen`
parameter of
[`sim_branching_process()`](https://ai4ci.github.io/ggoutbreak/reference/sim_branching_process.md)
to allow age group contract matrices to be applied for example (assuming
a categorical age).

## Usage

``` r
cfg_transition_fn(transition)
```

## Arguments

- transition:

  a transition matrix or long format dataframe. If a matrix the columns
  should add to 1, and the column names are the input class. row names
  are the output class. the data frame format must have input, output,
  and probability columns.

## Value

a function that given an input will return samples of the output class
according to the probability distributions.

## Examples

``` r
age = rep(c("child","adult","elderly"),100)

fn = cfg_transition_fn(dplyr::tribble(
  ~input, ~output, ~probability,
  "child", "child", 0.5,
  "child", "adult", 0.5,
  "adult", "child", 0.5,
  "adult", "adult", 0.3,
  "adult", "elderly", 0.2,
  "elderly","elderly", 0.5,
  "elderly","adult", 0.5,
))

table(fn(age),age)
#>          age
#>           adult child elderly
#>   adult      33    37      63
#>   child      48    63       0
#>   elderly    19     0      37
```
