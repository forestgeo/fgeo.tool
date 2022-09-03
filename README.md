---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Import and manipulate ForestGEO data

<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Coverage status](https://coveralls.io/repos/github/forestgeo/fgeo.tool/badge.svg)](https://coveralls.io/github/forestgeo/fgeo.tool)
[![CRAN status](https://www.r-pkg.org/badges/version/fgeo.tool)](https://cran.r-project.org/package=fgeo.tool)
[![R-CMD-check](https://github.com/forestgeo/fgeo.tool/workflows/R-CMD-check/badge.svg)](https://github.com/forestgeo/fgeo.tool/actions)
[![Codecov test coverage](https://codecov.io/gh/forestgeo/fgeo.tool/branch/master/graph/badge.svg)](https://app.codecov.io/gh/forestgeo/fgeo.tool?branch=master)
[![R-CMD-check](https://github.com/forestgeo/fgeo.tool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/forestgeo/fgeo.tool/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

__fgeo.tool__ helps you to import and manipulate [ForestGEO](<https://forestgeo.si.edu/>) data.

## Installation

Install the latest stable version of **fgeo.tool** from CRAN with:

```R
install.packages("fgeo.tool")
```

Install the development version of **fgeo.tool** from GitHub with:

```R
# install.packages("devtools")
devtools::install_github("forestgeo/fgeo.tool")
```

Or [install all **fgeo** packages in one step](https://forestgeo.github.io/fgeo//index.html#installation).

## Example


```r
library(fgeo.tool)
#> Error in library(fgeo.tool): there is no package called 'fgeo.tool'
# Helps access data for examples
library(fgeo.x)
```

`example_path()` allows you to access datasets stored in your R libraries.


```r
example_path()
#>  [1] "csv"           "mixed_files"   "rdata"         "rdata_one"    
#>  [5] "rds"           "taxa.csv"      "tsv"           "vft_4quad.csv"
#>  [9] "view"          "weird"         "xl"

(vft_file <- example_path("view/vft_4quad.csv"))
#> [1] "/usr/local/lib/R/site-library/fgeo.x/extdata/view/vft_4quad.csv"
```

`read_vft()` and `read_taxa()` import a ViewFullTable and ViewTaxonomy from .tsv or .csv files.


```r
read_vft(vft_file)
#> Error in read_vft(vft_file): could not find function "read_vft"
```

`pick_dbh_under()`, `drop_status()` and friends pick and drop rows from a ForestGEO ViewFullTable or census table.


```r
tree5 <- fgeo.x::tree5

tree5 %>% 
  pick_dbh_under(100)
#> Error in tree5 %>% pick_dbh_under(100): could not find function "%>%"
```

`pick_main_stem()` and `pick_main_stemid()` pick the main stem or main stemid(s) of each tree in each census.


```r
stem <- download_data("luquillo_stem6_random")

dim(stem)
#> [1] 1320   19
dim(pick_main_stem(stem))
#> Error in pick_main_stem(stem): could not find function "pick_main_stem"
```

`add_status_tree()` adds the column status_tree based on the status of all stems of each tree.


```r
stem %>% 
  select(CensusID, treeID, stemID, status) %>% 
  add_status_tree()
#> Error in stem %>% select(CensusID, treeID, stemID, status) %>% add_status_tree(): could not find function "%>%"
```

`add_index()` and friends add columns to a ForestGEO-like dataframe.


```r
stem %>% 
  select(gx, gy) %>% 
  add_index()
#> Error in stem %>% select(gx, gy) %>% add_index(): could not find function "%>%"
```

[Get started with __fgeo__](https://forestgeo.github.io/fgeo/)

## Information

* [Getting help](https://forestgeo.github.io/fgeo.tool/SUPPORT.html).
* [Contributing](https://forestgeo.github.io/fgeo.tool/CONTRIBUTING.html).
* [Contributor Code of Conduct](https://forestgeo.github.io/fgeo.tool/CODE_OF_CONDUCT.html).
