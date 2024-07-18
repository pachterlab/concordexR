
<!-- README.md is generated from README.Rmd. Please edit that file -->

# concordexR

<!-- badges: start -->

[![codecov](https://codecov.io/gh/pachterlab/concordexR/branch/main/graph/badge.svg?token=FSASJPR4T5)](https://codecov.io/gh/pachterlab/concordexR)
<!-- badges: end -->

The goal of concordexR is to identify spatial homogeneous regions (SHRs) as defined in the recent manuscrpt["Identification of spatial homogenous regions in tissues with concordex"](https://doi.org/10.1101/2023.06.28.546949). Briefly, SHRs are are domains that are homogeneous with respect to cell type composition. concordex relies on the the k-nearest-neighbor (kNN) graph to representing similarities between cells and uses common clustering algorithms to identify SHRs.

## Installation

You can install the development version of concordexR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pachterlab/concordexR")
```

## Example

This is a basic example using concordex:

``` r
library(concordexR)
```

``` r
res <- calculateConcordex(g$index, labels = iris$Species, k = 10, return.map = TRUE)
```

``` r
plotConcordexSim(res)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="70%" />

``` r
heatConcordex(res)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="70%" />
