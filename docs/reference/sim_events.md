# Extract the events dataframe from a simulation output

All the simulations should include details of major changes in the
simulation input parameters, particularly for step functions. This set
of events can be directly represented on a `ggoutbreak` plot using the
`events` parameter

## Usage

``` r
sim_events(df)
```

## Arguments

- df:

  the output of a `ggoutbreak` simulation

## Value

an events dataframe

## Examples

``` r
sim_events(example_poisson_rt())
#> # A tibble: 2 × 4
#> # Groups:   statistic [1]
#>   statistic  label   start      end  
#>   <chr>      <chr>   <date>     <lgl>
#> 1 infections Rt=2.50 2025-01-01 NA   
#> 2 infections Rt=0.80 2025-02-10 NA   
```
