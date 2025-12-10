# ggoutbreak

Simple statistical models and visualisations for calculating the
incidence, proportion, exponential growth rate, and reproduction number
of infectious disease case time series. This tool kit was largely
developed during the COVID-19 pandemic.

## Installation

`ggoutbreak` is hosted on the [AI4CI
r-universe](https://ai4ci.r-universe.dev/). Installation from there is
as follows:

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

## Funding

The authors gratefully acknowledge the support of the UK Research and
Innovation AI programme of the Engineering and Physical Sciences
Research Council [EPSRC grant
EP/Y028392/1](https://gtr.ukri.org/projects?ref=EP%2FY028392%2F1).
