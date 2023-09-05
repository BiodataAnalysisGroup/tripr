
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- TODO: badges -->

<!-- badges: start -->

<!-- [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable) -->
<!-- [![BioC status](http://bioconductor.org/shields/build/release/data-experiment/spatialLIBD.svg)](http://bioconductor.org/checkResults/release/data-experiment-LATEST/spatialLIBD/) -->
<!-- [![BioC dev status](http://bioconductor.org/shields/build/devel/data-experiment/spatialLIBD.svg)](http://bioconductor.org/checkResults/devel/data-experiment-LATEST/spatialLIBD/) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/BiodataAnalysisGroup/tripr/branch/master/graph/badge.svg)](https://codecov.io/gh/BiodataAnalysisGroup/tripr?branch=master) -->

<!-- [![Support site activity, last 6 months: tagged questions/avg. answers per question/avg. comments per question/accepted answers, or 0 if no tagged posts.](http://www.bioconductor.org/shields/posts/spatialLIBD.svg)](https://support.bioconductor.org/t/spatialLIBD/) -->
<!-- [![GitHub issues](https://img.shields.io/github/issues/BiodataAnalysisGroup/tripr)](https://github.com/BiodataAnalysisGroup/tripr/issues) -->
<!-- [![DOI](https://zenodo.org/badge/225913568.svg)](https://zenodo.org/badge/latestdoi/225913568) -->

<!-- badges: end -->

# tripr <img src="inst/app/www/tripr.png" align="right" width="200" />

T-cell Receptor/Immunoglobulin Profiler (TRIP)

[![R-CMD-check-bioc](https://github.com/BiodataAnalysisGroup/tripr/actions/workflows/check-bioc.yml/badge.svg)](https://github.com/BiodataAnalysisGroup/tripr/actions/workflows/check-bioc.yml)

## Description

`tripr` is a [Bioconductor](http://bioconductor.org) package, written in
[shiny](https://shiny.rstudio.com/) that provides analytics services on
antigen receptor (B cell receptor immunoglobulin, BcR IG | T cell
receptor, TR) gene sequence data. Every step of the analysis can be
performed interactively, thus not requiring any programming skills. It
takes as input the output files of the [IMGT/HighV-Quest
tool](http://www.imgt.org/HighV-QUEST/home.action). Users can select to
analyze the data from each of the input samples separately, or the
combined data files from all samples and visualize the results
accordingly.

Functions for an `R` command-line use are also available.

## Installation

<!-- When accepted in Bioconductor -->

`tripr` is distributed as a [Bioconductor](https://www.bioconductor.org/) 
package and requires `R` (version "4.2"), which can be installed on any 
operating system from [CRAN](https://cran.r-project.org/), and 
Bioconductor (version "3.15").

To install `tripr` package enter the following commands in your `R` session:


```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("tripr")

## Check that you have a valid Bioconductor installation
BiocManager::valid()
```


## Launching the app

Once `tripr` is successfully installed, it can be loaded as follow:

``` r
library(tripr)
```

## User Guide

To view documentation for the version of this package installed in your system, 
start R and enter: 

``` r
browseVignettes("tripr")
```
