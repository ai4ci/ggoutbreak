# Plot an infectivity profile

Plot an infectivity profile

## Usage

``` r
plot_ip(ip = i_empirical_ip, alpha = NULL, ...)
```

## Arguments

- ip:

  A long format infectivity profile - a dataframe with columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - a0 (double) - the beginning of the time period (in days)

  - a1 (double) - the end of the time period (in days)

  Minimally grouped by: boot (and other groupings allowed).

- alpha:

  the alpha value of the bootstrap lines

- ...:

  passed onto geom_segment for controlling line thickness, alpha etc.

## Value

an ggplot object

## Examples

``` r
if(interactive()) {
  plot_ip(example_ganyani_ip())
}
```
