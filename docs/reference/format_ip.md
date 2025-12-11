# Print a summary of an infectivity profile

Print a summary of an infectivity profile

## Usage

``` r
format_ip(ip = i_empirical_ip)
```

## Arguments

- ip:

  the infectivity profile to summarise. `a0` and `a1` columns are
  optional if `tau` is given. - a dataframe with columns:

  - boot (anything + default(1)) - a bootstrap identifier

  - probability (proportion) - the probability of new event during this
    period.

  - a0 (double) - the beginning of the time period (in days)

  - a1 (double) - the end of the time period (in days)

  Minimally grouped by: boot (and other groupings allowed).

## Value

an infectivity profile description

## Examples

``` r
format_ip(example_ganyani_ip())
#> [1] "PDF: mean: 5.2 [3.98 — 6.82]; sd: 2.06 [0.644 — 3.19]; 100 bootstraps"
```
