
# snackapp.usage

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: LGPL v3](https://img.shields.io/badge/License-LGPL_v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://lboro-climb.shinyapps.io/snackapp-usage/)
[![DOI](https://zenodo.org/badge/426941715.svg)](https://zenodo.org/badge/latestdoi/426941715)
<!-- badges: end -->

The goal of snackapp.usage is to provide code to analyse data pulled from the Snacktivity SnackApp. The application can be viewed online [here](https://lboro-climb.shinyapps.io/snackapp-usage/). 

## Installation

You can install the released version of snackapp.usage from [Github](https://github.com/lboro-climb/snackapp.usage/) with:

``` r
remotes::install_github("lboro-climb/snackapp.usage")
library(snackapp.usage)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(snackapp.usage)
snackapp.usage::run_app()
```

