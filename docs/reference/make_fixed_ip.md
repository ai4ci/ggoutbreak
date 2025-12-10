# Generate a simple discrete infectivity profile from a gamma distribution

Generate a simple discrete infectivity profile from a gamma distribution

## Usage

``` r
make_fixed_ip(mean, sd = sqrt(mean), epiestim_compat = FALSE)
```

## Arguments

- mean:

  The mean of a gamma distribution

- sd:

  The sd of a gamma distribution

- epiestim_compat:

  Should the discretisation support EpiEstim (i.e. `Pr(x<1) = 0`)

## Value

a discrete infectivity profile with 1 bootstrap

## Examples

``` r
tmp = make_fixed_ip(5,2)

if(interactive()) {
  plot_ip(tmp, alpha=1)
}
```
