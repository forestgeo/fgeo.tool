
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Import and manipulate ForestGEO data

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Coverage
status](https://coveralls.io/repos/github/forestgeo/fgeo.tool/badge.svg)](https://coveralls.io/github/forestgeo/fgeo.tool)
[![CRAN
status](https://www.r-pkg.org/badges/version/fgeo.tool)](https://cran.r-project.org/package=fgeo.tool)
[![R-CMD-check](https://github.com/forestgeo/fgeo.tool/workflows/R-CMD-check/badge.svg)](https://github.com/forestgeo/fgeo.tool/actions)
[![Codecov test
coverage](https://codecov.io/gh/forestgeo/fgeo.tool/branch/main/graph/badge.svg)](https://app.codecov.io/gh/forestgeo/fgeo.tool?branch=main)
[![R-CMD-check](https://github.com/forestgeo/fgeo.tool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/forestgeo/fgeo.tool/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**fgeo.tool** helps you to import and manipulate
[ForestGEO](https://forestgeo.si.edu/) data.

## Installation

Install the latest stable version of **fgeo.tool** from CRAN with:

``` r
install.packages("fgeo.tool")
```

Install the development version of **fgeo.tool** from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("forestgeo/fgeo.tool")
```

Or [install all **fgeo** packages in one
step](https://forestgeo.github.io/fgeo//index.html#installation).

## Example

``` r
library(fgeo.tool)
#> 
#> Attaching package: 'fgeo.tool'
#> The following object is masked from 'package:stats':
#> 
#>     filter
# Helps access data for examples
library(fgeo.x)
```

`example_path()` allows you to access datasets stored in your R
libraries.

``` r
example_path()
#>  [1] "csv"           "mixed_files"   "rdata"         "rdata_one"    
#>  [5] "rds"           "taxa.csv"      "tsv"           "vft_4quad.csv"
#>  [9] "view"          "weird"         "xl"

(vft_file <- example_path("view/vft_4quad.csv"))
#> [1] "/usr/local/lib/R/site-library/fgeo.x/extdata/view/vft_4quad.csv"
```

`read_vft()` and `read_taxa()` import a ViewFullTable and ViewTaxonomy
from .tsv or .csv files.

``` r
read_vft(vft_file)
#> # A tibble: 500 × 32
#>     DBHID PlotName PlotID Family Genus SpeciesName Mnemonic Subspecies SpeciesID
#>     <int> <chr>     <int> <chr>  <chr> <chr>       <chr>    <chr>          <int>
#>  1 385164 luquillo      1 Rubia… Psyc… brachiata   PSYBRA   <NA>             185
#>  2 385261 luquillo      1 Urtic… Cecr… schreberia… CECSCH   <NA>              74
#>  3 384600 luquillo      1 Rubia… Psyc… brachiata   PSYBRA   <NA>             185
#>  4 608789 luquillo      1 Rubia… Psyc… berteroana  PSYBER   <NA>             184
#>  5 388579 luquillo      1 Areca… Pres… acuminata   PREMON   <NA>             182
#>  6 384626 luquillo      1 Arali… Sche… morototoni  SCHMOR   <NA>             196
#>  7 410958 luquillo      1 Rubia… Psyc… brachiata   PSYBRA   <NA>             185
#>  8 385102 luquillo      1 Piper… Piper glabrescens PIPGLA   <NA>             174
#>  9 353163 luquillo      1 Areca… Pres… acuminata   PREMON   <NA>             182
#> 10 481018 luquillo      1 Salic… Case… arborea     CASARB   <NA>              70
#> # ℹ 490 more rows
#> # ℹ 23 more variables: SubspeciesID <chr>, QuadratName <chr>, QuadratID <int>,
#> #   PX <dbl>, PY <dbl>, QX <dbl>, QY <dbl>, TreeID <int>, Tag <chr>,
#> #   StemID <int>, StemNumber <int>, StemTag <int>, PrimaryStem <chr>,
#> #   CensusID <int>, PlotCensusNumber <int>, DBH <dbl>, HOM <dbl>,
#> #   ExactDate <date>, Date <int>, ListOfTSM <chr>, HighHOM <int>,
#> #   LargeStem <chr>, Status <chr>
```

`pick_dbh_under()`, `drop_status()` and friends pick and drop rows from
a ForestGEO ViewFullTable or census table.

``` r
tree5 <- fgeo.x::tree5

tree5 %>% 
  pick_dbh_under(100)
#> # A tibble: 18 × 19
#>    treeID stemID tag    StemTag sp     quadrat    gx    gy MeasureID CensusID
#>     <int>  <int> <chr>  <chr>   <chr>  <chr>   <dbl> <dbl>     <int>    <int>
#>  1   7624 160987 108958 175325  TRIPAL 722     139.  425.     486675        5
#>  2  19930 117849 123493 165576  CASARB 425      61.3 496.     471979        5
#>  3  31702  39793 22889  22889   SLOBER 304      53.8  73.8    447307        5
#>  4  35355  44026 27538  27538   SLOBER 1106    203.  110.     449169        5
#>  5  39705  48888 33371  33370   CASSYL 1010    184.  194.     451067        5
#>  6  57380 155867 66962  171649  SLOBER 1414    274.  279.     459427        5
#>  7  95656 129113 131519 131519  OCOLEU 402      79.7  22.8    474157        5
#>  8  96051 129565 132348 132348  HIRRUG 1403    278    40.6    474523        5
#>  9  96963 130553 134707 134707  TETBAL 610     114.  182.     475236        5
#> 10 115310 150789 165286 165286  MANBID 225      24.0 497.     483175        5
#> 11 121424 158579 170701 170701  CASSYL 811     146.  218.     484785        5
#> 12 121689 158871 171277 171277  INGLAU 515      84.2 285.     485077        5
#> 13 121953 159139 171809 171809  PSYBRA 1318    247.  354.     485345        5
#> 14 124522 162698 174224 174224  CASSYL 1411    279.  210.     488386        5
#> 15 125038 163236 175335 175335  CASSYL 822     153.  426.     488924        5
#> 16 126087     NA 177394 <NA>    CASARB 521      89.8 408.         NA       NA
#> 17 126803     NA 178513 <NA>    PSYBER 622     113.  426          NA       NA
#> 18 126934     NA 178763 <NA>    MICRAC 324      47   480.         NA       NA
#> # ℹ 9 more variables: dbh <dbl>, pom <chr>, hom <dbl>, ExactDate <date>,
#> #   DFstatus <chr>, codes <chr>, nostems <dbl>, status <chr>, date <dbl>
```

`pick_main_stem()` and `pick_main_stemid()` pick the main stem or main
stemid(s) of each tree in each census.

``` r
stem <- download_data("luquillo_stem6_random")

dim(stem)
#> [1] 1320   19
dim(pick_main_stem(stem))
#> [1] 1000   19
```

`add_status_tree()` adds the column status_tree based on the status of
all stems of each tree.

``` r
stem %>% 
  select(CensusID, treeID, stemID, status) %>% 
  add_status_tree()
#> # A tibble: 1,320 × 5
#>    CensusID treeID stemID status status_tree
#>       <int>  <int>  <int> <chr>  <chr>      
#>  1        6    104    143 A      A          
#>  2        6    119    158 A      A          
#>  3       NA    180    222 G      A          
#>  4       NA    180    223 G      A          
#>  5        6    180    224 G      A          
#>  6        6    180    225 A      A          
#>  7        6    602    736 A      A          
#>  8        6    631    775 A      A          
#>  9        6    647    793 A      A          
#> 10        6   1086   1339 A      A          
#> # ℹ 1,310 more rows
```

`add_index()` and friends add columns to a ForestGEO-like dataframe.

``` r
stem %>% 
  select(gx, gy) %>% 
  add_index()
#> Guessing: plotdim = c(320, 500)
#> * If guess is wrong, provide the correct argument `plotdim`
#> # A tibble: 1,320 × 3
#>       gx    gy index
#>    <dbl> <dbl> <dbl>
#>  1  10.3  245.    13
#>  2 183.   410.   246
#>  3 165.   410.   221
#>  4 165.   410.   221
#>  5 165.   410.   221
#>  6 165.   410.   221
#>  7 149.   414.   196
#>  8  38.3  245.    38
#>  9 143.   411.   196
#> 10  68.9  253.    88
#> # ℹ 1,310 more rows
```

[Get started with **fgeo**](https://forestgeo.github.io/fgeo/)

## Information

- [Getting help](https://forestgeo.github.io/fgeo.tool/SUPPORT.html).
- [Contributing](https://forestgeo.github.io/fgeo.tool/CONTRIBUTING.html).
- [Contributor Code of
  Conduct](https://forestgeo.github.io/fgeo.tool/CODE_OF_CONDUCT.html).
