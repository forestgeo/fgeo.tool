
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/m8FNhQR.png" align="right" height=88 /> Functions for general purposes

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/forestgeo/fgeo.tool.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.tool)
[![Coverage
status](https://coveralls.io/repos/github/forestgeo/fgeo.tool/badge.svg)](https://coveralls.io/r/forestgeo/fgeo.tool?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/fgeo.tool)](https://cran.r-project.org/package=fgeo.tool)

The goal of **fgeo.tool** is to provide functions for general purposes.
Many of its functions are used in multiple other **fgeo** packages so
**fgeo.tool** acts as a central repository of code. In particular, the
packages. For general porpose functions with no expternal dependency see
[**fgeo.base**](https://forestgeo.github.io/fgeo.base/).

## Installation

[Install all **fgeo** packages in one
step](https://forestgeo.github.io/fgeo/index.html#installation)

    # install.packages("remotes")
    remotes::install_github("forestgeo/fgeo.tool")

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

``` r
library(fgeo.tool)

stem <- tibble::tribble(
  ~CensusID, ~treeID, ~stemID, ~status,
          1,       1,       1,     "A",
          1,       1,       2,     "D",
          # -- -- -- -- -- -- -- -- -- 
          1,       2,       3,     "D",
          1,       2,       4,     "D",
          # == == == == == == == == ==
          2,       1,       1,     "A",
          2,       1,       2,     "G",
          # -- -- -- -- -- -- -- -- -- 
          2,       2,       3,     "D",
          2,       2,       4,     "G"
)

# Determine the status of each tree based on the status of its stems
add_status_tree(stem)
#> # A tibble: 8 x 5
#>   CensusID treeID stemID status status_tree
#>      <dbl>  <dbl>  <dbl> <chr>  <chr>      
#> 1        1      1      1 A      A          
#> 2        1      1      2 D      A          
#> 3        1      2      3 D      D          
#> 4        1      2      4 D      D          
#> 5        2      1      1 A      A          
#> 6        2      1      2 G      A          
#> 7        2      2      3 D      A          
#> 8        2      2      4 G      A
```

[Get started with
**fgeo**](https://forestgeo.github.io/fgeo/articles/fgeo.html)

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgements

Thanks to all partners of ForestGEO, for sharing their ideas and code.
