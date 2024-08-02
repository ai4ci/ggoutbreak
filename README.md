# ggoutbreak


<!-- badges: start -->
[![R-CMD-check](https://github.com/ai4ci/ggoutbreak/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ai4ci/ggoutbreak/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/836807064.svg)](https://zenodo.org/doi/10.5281/zenodo.13165560)
[![ggoutbreak status badge](https://ai4ci.r-universe.dev/badges/ggoutbreak)](https://ai4ci.r-universe.dev)
<!-- badges: end -->


Simple statistical models and visualisations for calculating the 
incidence, proportion, exponential growth rate, and reproduction number of 
infectious disease case time series. This tool kit was largely developed during 
the COVID-19 pandemic.

## Installation

`ggoutbreak` is hosted on the [AI4CI r-universe](https://ai4ci.r-universe.dev/).
Installation from there is as follows:

``` r
options(repos = c(
  "ai4ci" = 'https://ai4ci.r-universe.dev/',
  CRAN = 'https://cloud.r-project.org'))

# Download and install ggoutbreak in R
install.packages("ggoutbreak")
```

You can install the development version of `ggoutbreak` from
[GitHub](https://github.com/ai4ci/ggoutbreak) with:

``` r
# install.packages("devtools")
devtools::install_github("ai4ci/ggoutbreak")
```
