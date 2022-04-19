
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scTernary

<!-- badges: start -->
<!-- badges: end -->

The goal of scTernary is to perform ternary plot analysis for single
cell RNA-seq data.

## Installation

You can install the released version of scTernary from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("scTernary")
devtools::install_github("jinming-cheng/scTernary")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(scTernary)

data_for_ternary = data.frame(Basal = c(1,2,2,0,0,1,0,1,0),
                              ML    = c(0,1,0,1,2,2,0,0,1),
                              LP    = c(0,0,1,0,1,0,1,2,2) )

vcdTernaryPlot(data = data_for_ternary,
               group = rep(c("Bas","ML","LP"),each=3),
               group_levels = c("Bas","LP","ML"),
               group_color = c("red","green","blue"),
               point_size = 1,
               legend_point_size = 0.6)
```

<img src="man/figures/README-example-1.png" width="100%" />
