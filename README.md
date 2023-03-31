
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nomapR

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/kayla-jackson/nomapR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/kayla-jackson/nomapR?branch=master)
<!-- badges: end -->

The goal of nomapR is to replace UMAP as a clustering diagnostic.

## Installation

You can install the development version of nomapR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kayla-jackson/nomapR")
```

## Example

This is a basic example using nomap:

``` r
library(nomapR)
library(BiocNeighbors)
```

``` r
g <- findKNN(iris[,seq_len(4)], k = 10)
#> Warning in (function (to_check, X, clust_centers, clust_info, dtype, nn, :
#> detected tied distances to neighbors, see ?'BiocNeighbors-ties'
res <- calculateNomap(g$index, labels = iris$Species, k = 10, return.map = TRUE)
```

``` r
plotNomapSim(res)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="70%" />

``` r
heatNomap(res)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="70%" />
