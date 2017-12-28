
<!-- README.md is generated from README.Rmd. Please edit that file -->
fgeo.utils: Utility functions of ForestGEO <img src="https://i.imgur.com/39pvr4n.png" align="right" height=44 />
================================================================================================================

[![Travis build status](https://travis-ci.org/forestgeo/fgeo.utils.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.utils) [![Coverage status](https://coveralls.io/repos/github/forestgeo/fgeo.utils/badge.svg)](https://coveralls.io/r/forestgeo/fgeo.utils?branch=master) [![CRAN status](http://www.r-pkg.org/badges/version/fgeo.utils)](https://cran.r-project.org/package=fgeo.utils)

Installation
------------

You can install fgeo.utils from github with:

``` r
# install.packages("remotes")
remotes::install_github("forestgeo/fgeo.utils")
library(fgeo.utils)
```

Example
-------

``` r
library(fgeo.utils)
add_status_tree(bciex::bci12vft_mini)
#> # A tibble: 4,374 x 29
#>    MeasureID PlotID  Plot        Family           GenusSpecies      Genus
#>        <int>  <int> <chr>         <chr>                  <chr>      <chr>
#>  1         2      1   bci Lecythidaceae       Gustavia superba   Gustavia
#>  2         3      1   bci Myristicaceae    Virola surinamensis     Virola
#>  3         4      1   bci     Malvaceae Quararibea asterolepis Quararibea
#>  4         5      1   bci   Burseraceae    Protium tenuifolium    Protium
#>  5         6      1   bci      Moraceae    Brosimum alicastrum   Brosimum
#>  6         7      1   bci   Burseraceae    Protium tenuifolium    Protium
#>  7         8      1   bci     Malvaceae Quararibea asterolepis Quararibea
#>  8         9      1   bci     Malvaceae Quararibea asterolepis Quararibea
#>  9        10      1   bci     Lauraceae          Ocotea whitei     Ocotea
#> 10        11      1   bci    Annonaceae    Guatteria dumetorum  Guatteria
#> # ... with 4,364 more rows, and 23 more variables: SpeciesName <chr>,
#> #   SubSpeciesName <chr>, SpeciesID <int>, Mnemonic <chr>,
#> #   QuadratID <int>, QuadratName <chr>, x <dbl>, y <dbl>, gx <dbl>,
#> #   gy <dbl>, TreeID <int>, Tag <chr>, StemID <int>, StemTag <chr>,
#> #   PrimaryStem <chr>, CensusID <int>, PlotCensusNumber <int>, DBH <int>,
#> #   HOM <dbl>, ExactDate <date>, ListOfTSM <chr>, Status <chr>,
#> #   status_tree <chr>
```
