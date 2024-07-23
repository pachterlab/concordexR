
<!-- README.md is generated from README.Rmd. Please edit that file -->

# concordexR

<!-- badges: start -->

[![codecov](https://codecov.io/gh/pachterlab/concordexR/branch/main/graph/badge.svg?token=FSASJPR4T5)](https://codecov.io/gh/pachterlab/concordexR)
<!-- badges: end -->

The goal of concordexR is to identify spatial homogeneous regions (SHRs)
as defined in the recent manuscript, [“Identification of spatial
homogenous regions in tissues with
concordex”](https://doi.org/10.1101/2023.06.28.546949). Briefly, SHRs
are are domains that are homogeneous with respect to cell type
composition. `concordex` relies on the the k-nearest-neighbor (kNN)
graph to representing similarities between cells and uses common
clustering algorithms to identify SHRs.

## Installation

(Recommended) You can install the development version of `concordexR`
from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pachterlab/concordexR")
#> Using GitHub PAT from the git credential store.
#> Skipping install of 'concordexR' from a github remote, the SHA1 (046127e9) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

Versions of the `concordexR` package that do not enable clustering
spatial data into spatial homogeneous regions (SHRs) are available for
Bioconductor versions 3.17-19. The most recent version of the package is
slated to be released on Bioconductor version 3.20. You can install the
Bioconductor development version with:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
    
BiocManager::install(version="devel")
BiocManager::install("concordexR")
```

## Example

This is a basic example using concordex:

``` r
library(SFEData)
library(Voyager)
#> Loading required package: SpatialFeatureExperiment
#> 
#> Attaching package: 'SpatialFeatureExperiment'
#> The following object is masked from 'package:base':
#> 
#>     scale
library(scran)
#> Loading required package: SingleCellExperiment
#> Loading required package: SummarizedExperiment
#> Loading required package: MatrixGenerics
#> Loading required package: matrixStats
#> 
#> Attaching package: 'MatrixGenerics'
#> The following objects are masked from 'package:matrixStats':
#> 
#>     colAlls, colAnyNAs, colAnys, colAvgsPerRowSet, colCollapse,
#>     colCounts, colCummaxs, colCummins, colCumprods, colCumsums,
#>     colDiffs, colIQRDiffs, colIQRs, colLogSumExps, colMadDiffs,
#>     colMads, colMaxs, colMeans2, colMedians, colMins, colOrderStats,
#>     colProds, colQuantiles, colRanges, colRanks, colSdDiffs, colSds,
#>     colSums2, colTabulates, colVarDiffs, colVars, colWeightedMads,
#>     colWeightedMeans, colWeightedMedians, colWeightedSds,
#>     colWeightedVars, rowAlls, rowAnyNAs, rowAnys, rowAvgsPerColSet,
#>     rowCollapse, rowCounts, rowCummaxs, rowCummins, rowCumprods,
#>     rowCumsums, rowDiffs, rowIQRDiffs, rowIQRs, rowLogSumExps,
#>     rowMadDiffs, rowMads, rowMaxs, rowMeans2, rowMedians, rowMins,
#>     rowOrderStats, rowProds, rowQuantiles, rowRanges, rowRanks,
#>     rowSdDiffs, rowSds, rowSums2, rowTabulates, rowVarDiffs, rowVars,
#>     rowWeightedMads, rowWeightedMeans, rowWeightedMedians,
#>     rowWeightedSds, rowWeightedVars
#> Loading required package: GenomicRanges
#> Loading required package: stats4
#> Loading required package: BiocGenerics
#> 
#> Attaching package: 'BiocGenerics'
#> The following objects are masked from 'package:stats':
#> 
#>     IQR, mad, sd, var, xtabs
#> The following objects are masked from 'package:base':
#> 
#>     anyDuplicated, aperm, append, as.data.frame, basename, cbind,
#>     colnames, dirname, do.call, duplicated, eval, evalq, Filter, Find,
#>     get, grep, grepl, intersect, is.unsorted, lapply, Map, mapply,
#>     match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
#>     Position, rank, rbind, Reduce, rownames, sapply, setdiff, table,
#>     tapply, union, unique, unsplit, which.max, which.min
#> Loading required package: S4Vectors
#> 
#> Attaching package: 'S4Vectors'
#> The following object is masked from 'package:utils':
#> 
#>     findMatches
#> The following objects are masked from 'package:base':
#> 
#>     expand.grid, I, unname
#> Loading required package: IRanges
#> Loading required package: GenomeInfoDb
#> Loading required package: Biobase
#> Welcome to Bioconductor
#> 
#>     Vignettes contain introductory material; view with
#>     'browseVignettes()'. To cite Bioconductor, see
#>     'citation("Biobase")', and for packages 'citation("pkgname")'.
#> 
#> Attaching package: 'Biobase'
#> The following object is masked from 'package:MatrixGenerics':
#> 
#>     rowMedians
#> The following objects are masked from 'package:matrixStats':
#> 
#>     anyMissing, rowMedians
#> Loading required package: scuttle
library(bluster)

library(concordexR)
```

``` r
sfe <- McKellarMuscleData("small")
#> see ?SFEData and browseVignettes('SFEData') for documentation
#> loading from cache
clusters <- quickCluster(sfe, min.size=2, d=15)
```

``` r
nbc <- calculateConcordex(
    sfe,
    clusters,
    n_neighbors=10,
    BLUSPARAM=KmeansParam(2)
)

colData(sfe)[["shr"]] <- attr(nbc, "shrs")
```

``` r
plotSpatialFeature(sfe, features="shr")
```

<img src="man/figures/README-plot-shr-1.png" width="70%" />

## Citation

If you’d like to use the `concordexR` package in your research, please
cite our recent bioRxiv preprint

> Jackson, K.; Booeshaghi, A. S.; Gálvez-Merchán, Á.; Moses, L.; Chari,
> T.; Pachter, L. Quantitative assessment of single-cell RNA-seq
> clustering with CONCORDEX. bioRxiv (Cold Spring Harbor Laboratory)
> 2023. <https://doi.org/10.1101/2023.06.28.546949>.

@article {Jackson2023.06.28.546949, author = {Jackson, Kayla C. and
Booeshaghi, A. Sina and G{'a}lvez-Merch{'a}n, {'A}ngel and Moses, Lambda
and Chari, Tara and Kim, Alexandra and Pachter, Lior}, title =
{Identification of spatial homogeneous regions in tissues with
concordex}, year = {2024}, doi = {10.1101/2023.06.28.546949}, publisher
= {Cold Spring Harbor Laboratory}, URL =
{<https://www.biorxiv.org/content/early/2024/07/18/2023.06.28.546949>},
journal = {bioRxiv} }

## Maintainer

[Kayla Jackson](https://github.com/kayla-jackson)
