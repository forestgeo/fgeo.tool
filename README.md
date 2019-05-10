
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Import and manipulate ForestGEO data

**fgeo.tool** helps you to import and manipulate
[ForestGEO](http://www.forestgeo.si.edu/) data.

## Installation

Install the latest stable version of **fgeo.tool** with:

``` r
these_repos <- c(getOption("repos"), "https://forestgeo.github.io/drat")
install.packages("fgeo.tool", repos = these_repos)
```

Install the development version of **fgeo.tool** with:

``` r
# install.packages("devtools")
devtools::install_github("forestgeo/fgeo.tool")
```

Or [install all **fgeo** packages in one
step](https://forestgeo.github.io/fgeo/index.html#installation).

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
#> [1] "C:/Users/LeporeM/Documents/R/win-library/3.6/fgeo.x/extdata/view/vft_4quad.csv"
```

`read_vft()` and `read_taxa()` import a ViewFullTable and ViewTaxonomy
from .tsv or .csv files.

``` r
read_vft(vft_file)
#> # A tibble: 500 x 32
#>     DBHID PlotName PlotID Family Genus SpeciesName Mnemonic Subspecies
#>     <int> <chr>     <int> <chr>  <chr> <chr>       <chr>    <chr>     
#>  1 385164 luquillo      1 Rubia~ Psyc~ brachiata   PSYBRA   <NA>      
#>  2 385261 luquillo      1 Urtic~ Cecr~ schreberia~ CECSCH   <NA>      
#>  3 384600 luquillo      1 Rubia~ Psyc~ brachiata   PSYBRA   <NA>      
#>  4 608789 luquillo      1 Rubia~ Psyc~ berteroana  PSYBER   <NA>      
#>  5 388579 luquillo      1 Areca~ Pres~ acuminata   PREMON   <NA>      
#>  6 384626 luquillo      1 Arali~ Sche~ morototoni  SCHMOR   <NA>      
#>  7 410958 luquillo      1 Rubia~ Psyc~ brachiata   PSYBRA   <NA>      
#>  8 385102 luquillo      1 Piper~ Piper glabrescens PIPGLA   <NA>      
#>  9 353163 luquillo      1 Areca~ Pres~ acuminata   PREMON   <NA>      
#> 10 481018 luquillo      1 Salic~ Case~ arborea     CASARB   <NA>      
#> # ... with 490 more rows, and 24 more variables: SpeciesID <int>,
#> #   SubspeciesID <chr>, QuadratName <chr>, QuadratID <int>, PX <dbl>,
#> #   PY <dbl>, QX <dbl>, QY <dbl>, TreeID <int>, Tag <chr>, StemID <int>,
#> #   StemNumber <int>, StemTag <int>, PrimaryStem <chr>, CensusID <int>,
#> #   PlotCensusNumber <int>, DBH <dbl>, HOM <dbl>, ExactDate <date>,
#> #   Date <int>, ListOfTSM <chr>, HighHOM <int>, LargeStem <chr>,
#> #   Status <chr>
```

`pick_dbh_under()`, `drop_status()` and friends pick and drop rows from
a ForestGEO ViewFullTable or census table.

``` r
tree5 <- fgeo.x::tree5

tree5 %>% 
  pick_dbh_under(100)
#> # A tibble: 18 x 19
#>    treeID stemID tag   StemTag sp    quadrat    gx    gy MeasureID CensusID
#>     <int>  <int> <chr> <chr>   <chr> <chr>   <dbl> <dbl>     <int>    <int>
#>  1   7624 160987 1089~ 175325  TRIP~ 722     139.  425.     486675        5
#>  2  19930 117849 1234~ 165576  CASA~ 425      61.3 496.     471979        5
#>  3  31702  39793 22889 22889   SLOB~ 304      53.8  73.8    447307        5
#>  4  35355  44026 27538 27538   SLOB~ 1106    203.  110.     449169        5
#>  5  39705  48888 33371 33370   CASS~ 1010    184.  194.     451067        5
#>  6  57380 155867 66962 171649  SLOB~ 1414    274.  279.     459427        5
#>  7  95656 129113 1315~ 131519  OCOL~ 402      79.7  22.8    474157        5
#>  8  96051 129565 1323~ 132348  HIRR~ 1403    278    40.6    474523        5
#>  9  96963 130553 1347~ 134707  TETB~ 610     114.  182.     475236        5
#> 10 115310 150789 1652~ 165286  MANB~ 225      24.0 497.     483175        5
#> 11 121424 158579 1707~ 170701  CASS~ 811     146.  218.     484785        5
#> 12 121689 158871 1712~ 171277  INGL~ 515      84.2 285.     485077        5
#> 13 121953 159139 1718~ 171809  PSYB~ 1318    247.  354.     485345        5
#> 14 124522 162698 1742~ 174224  CASS~ 1411    279.  210.     488386        5
#> 15 125038 163236 1753~ 175335  CASS~ 822     153.  426.     488924        5
#> 16 126087     NA 1773~ <NA>    CASA~ 521      89.8 408.         NA       NA
#> 17 126803     NA 1785~ <NA>    PSYB~ 622     113.  426          NA       NA
#> 18 126934     NA 1787~ <NA>    MICR~ 324      47   480.         NA       NA
#> # ... with 9 more variables: dbh <dbl>, pom <chr>, hom <dbl>,
#> #   ExactDate <date>, DFstatus <chr>, codes <chr>, nostems <dbl>,
#> #   status <chr>, date <dbl>
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

`add_status_tree()` adds the column status\_tree based on the status of
all stems of each tree.

``` r
stem %>% 
  select(CensusID, treeID, stemID, status) %>% 
  add_status_tree()
#> # A tibble: 1,320 x 5
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
#> # ... with 1,310 more rows
```

`add_index()` and friends add columns to a ForestGEO-like dataframe.

``` r
stem %>% 
  select(gx, gy) %>% 
  add_index()
#> Guessing: plotdim = c(320, 500)
#> * If guess is wrong, provide the correct argument `plotdim`
#> # A tibble: 1,320 x 3
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
#> # ... with 1,310 more rows
```

[Get started with **fgeo**](https://forestgeo.github.io/fgeo)

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
