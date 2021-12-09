
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pfocal <img src='man/figures/logo.png' align="right" height="150" />

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/LandSciTech/pfocal/workflows/R-CMD-check/badge.svg)](https://github.com/LandSciTech/pfocal/actions)
[![Codecov test
coverage](https://codecov.io/gh/LandSciTech/pfocal/branch/master/graph/badge.svg)](https://codecov.io/gh/LandSciTech/pfocal?branch=master)
<!-- badges: end -->

The goal of `pfocal` is to implement fast parallel convolution. R
version \>= 4.0.0 is required.

## Installation

You can install the package as such:

``` r
# From CRAN
install.packages("devtools")

# Dev version from GitHub
devtools::install_github("LandSciTech/pfocal")
```

## Example

``` r
library(pfocal)

data <- matrix(nrow = 100, ncol = 100, 
               data = runif(n = 100*100, min = 0, max = 10))
image(data, asp = 1)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
kernel <- exponential_kernel()
convoluted <- pfocal(data = data, kernel = kernel, edge_value = 0)
image(convoluted, asp = 1)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
