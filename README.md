
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Functions for general purposes

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/forestgeo/fgeo.tool.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.tool)
[![Coverage
status](https://coveralls.io/repos/github/forestgeo/fgeo.tool/badge.svg)](https://coveralls.io/r/forestgeo/fgeo.tool?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/fgeo.tool)](https://cran.r-project.org/package=fgeo.tool)

The goal of **fgeo.tool** is to provide functions for general purposes.
Many of its functions are used in multiple other **fgeo** packages.

## Installation

Install the pre-release version of **fgeo.tool**:

    # install.packages("devtools")
    devtools::install_github("forestgeo/fgeo.tool@pre-release")

Or install the development version of **fgeo.tool**:

    # install.packages("devtools")
    devtools::install_github("forestgeo/fgeo.tool")

Or [install all **fgeo** packages in one
step](https://forestgeo.github.io/fgeo/index.html#installation).

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

``` r
library(fgeo.tool)
#> 
#> Attaching package: 'fgeo.tool'
#> The following object is masked from 'package:stats':
#> 
#>     filter
```

Read multiple censuses from a directory into a list of dataframes.

``` r
dir_multi_rdata <- tool_example("rdata")
dir(dir_multi_rdata)
#> [1] "tree5.RData" "tree6.RData"

census56_list <- rdata_list(dir_multi_rdata)
census56_list
#> $tree5
#> # A tibble: 1,000 x 19
#>    treeID stemID tag   StemTag sp    quadrat    gx    gy MeasureID CensusID
#>     <int>  <int> <chr> <chr>   <chr> <chr>   <dbl> <dbl>     <int>    <int>
#>  1    104    143 10009 10009   DACE~ 113      10.3  245.    439947        5
#>  2    119    158 1001~ 100104  MYRS~ 1021    183.   410.    466597        5
#>  3    180    225 1001~ 100174  CASA~ 921     165.   410.    466623        5
#>  4    602    736 1006~ 100649  GUAG~ 821     149.   414.    466727        5
#>  5    631    775 10069 10069   PREM~ 213      38.3  245.    439989        5
#>  6    647    793 1007~ 100708  SCHM~ 821     143.   411.    466743        5
#>  7   1086   1339 10122 10122   DRYG~ 413      68.9  253.    440021        5
#>  8   1144   1410 1012~ 101285  SCHM~ 920     161.   395.    466889        5
#>  9   1168   1438 10131 10131   DACE~ 413      70.6  252.    440031        5
#> 10   1380 114352 1015~ 149529  CASA~ 820     142.   386.    466957        5
#> # ... with 990 more rows, and 9 more variables: dbh <dbl>, pom <chr>,
#> #   hom <dbl>, ExactDate <date>, DFstatus <chr>, codes <chr>,
#> #   nostems <dbl>, status <chr>, date <dbl>
#> 
#> $tree6
#> # A tibble: 1,000 x 19
#>    treeID stemID tag   StemTag sp    quadrat    gx    gy MeasureID CensusID
#>     <int>  <int> <chr> <chr>   <chr> <chr>   <dbl> <dbl>     <int>    <int>
#>  1    104    143 10009 10009   DACE~ 113      10.3  245.    582850        6
#>  2    119    158 1001~ 100104  MYRS~ 1021    183.   410.    578696        6
#>  3    180    225 1001~ 100174  CASA~ 921     165.   410.    617049        6
#>  4    602    736 1006~ 100649  GUAG~ 821     149.   414.    614253        6
#>  5    631    775 10069 10069   PREM~ 213      38.3  245.    598429        6
#>  6    647    793 1007~ 100708  SCHM~ 821     143.   411.    614211        6
#>  7   1086   1339 10122 10122   DRYG~ 413      68.9  253.    603131        6
#>  8   1144   1410 1012~ 101285  SCHM~ 920     161.   395.    616923        6
#>  9   1168   1438 10131 10131   DACE~ 413      70.6  252.    603151        6
#> 10   1380   1702 1015~ 101560  CASA~ 820     142.   386.    614023        6
#> # ... with 990 more rows, and 9 more variables: dbh <dbl>, pom <chr>,
#> #   hom <dbl>, ExactDate <date>, DFstatus <chr>, codes <chr>,
#> #   nostems <dbl>, status <chr>, date <dbl>
```

Collapse all censuses into a single dataframe.

``` r
census56_df <- list_df(census56_list)
census56_df
#> # A tibble: 2,000 x 19
#>    treeID stemID tag   StemTag sp    quadrat    gx    gy MeasureID CensusID
#>     <int>  <int> <chr> <chr>   <chr> <chr>   <dbl> <dbl>     <int>    <int>
#>  1    104    143 10009 10009   DACE~ 113      10.3  245.    439947        5
#>  2    119    158 1001~ 100104  MYRS~ 1021    183.   410.    466597        5
#>  3    180    225 1001~ 100174  CASA~ 921     165.   410.    466623        5
#>  4    602    736 1006~ 100649  GUAG~ 821     149.   414.    466727        5
#>  5    631    775 10069 10069   PREM~ 213      38.3  245.    439989        5
#>  6    647    793 1007~ 100708  SCHM~ 821     143.   411.    466743        5
#>  7   1086   1339 10122 10122   DRYG~ 413      68.9  253.    440021        5
#>  8   1144   1410 1012~ 101285  SCHM~ 920     161.   395.    466889        5
#>  9   1168   1438 10131 10131   DACE~ 413      70.6  252.    440031        5
#> 10   1380 114352 1015~ 149529  CASA~ 820     142.   386.    466957        5
#> # ... with 1,990 more rows, and 9 more variables: dbh <dbl>, pom <chr>,
#> #   hom <dbl>, ExactDate <date>, DFstatus <chr>, codes <chr>,
#> #   nostems <dbl>, status <chr>, date <dbl>
```

Summarize by groups.

``` r
by_census <- group_by(census56_df, CensusID)
summarize(by_census, n = n_distinct(treeID))
#> Warning in summarise_impl(.data, dots): hybrid evaluation forced for
#> `n_distinct`. Please use dplyr::n_distinct() or library(dplyr) to remove
#> this warning.
#> # A tibble: 3 x 2
#>   CensusID     n
#>      <int> <int>
#> 1        5   957
#> 2        6  1000
#> 3       NA    43

# Same
census56_df %>% 
  group_by(CensusID) %>% 
  summarize(n = n_distinct(treeID))
#> Warning in summarise_impl(.data, dots): hybrid evaluation forced for
#> `n_distinct`. Please use dplyr::n_distinct() or library(dplyr) to remove
#> this warning.
#> # A tibble: 3 x 2
#>   CensusID     n
#>      <int> <int>
#> 1        5   957
#> 2        6  1000
#> 3       NA    43
```

Pick rows and reorganize columns.

``` r
census56_df %>% 
  pick_dbh_over(250) %>% 
  pick_status("A") %>%
  add_status_tree() %>% 
  select(dbh, contains("status"), everything())
#> Warning: No observation has .status = D
#>   * Detected values: A
#> # A tibble: 210 x 20
#>      dbh DFstatus status status_tree treeID stemID tag   StemTag sp   
#>    <dbl> <chr>    <chr>  <chr>        <int>  <int> <chr> <chr>   <chr>
#>  1   604 alive    A      A             1168   1438 10131 10131   DACE~
#>  2    NA broken ~ A      A             1380 114352 1015~ 149529  CASA~
#>  3   370 alive    A      A             1564   1910 10176 10176   HOMR~
#>  4    NA broken ~ A      A             1652   2026 1018~ 101867  OCOL~
#>  5   296 alive    A      A             3645   4426 1041~ 104113  SCHM~
#>  6   359 alive    A      A             4329   5283 1049  1049    INGL~
#>  7    NA broken ~ A      A             6677   8147 1077~ 107760  CECS~
#>  8   319 alive    A      A             7832   9768 1092~ 109231  TETB~
#>  9    NA stem de~ A      A             9689  12178 1115~ 111528  CORB~
#> 10    NA broken ~ A      A             9873 116236 1117~ 129228  MICP~
#> # ... with 200 more rows, and 11 more variables: quadrat <chr>, gx <dbl>,
#> #   gy <dbl>, MeasureID <int>, CensusID <int>, pom <chr>, hom <dbl>,
#> #   ExactDate <date>, codes <chr>, nostems <dbl>, date <dbl>
```

[Get started with
**fgeo**](https://forestgeo.github.io/fgeo/articles/fgeo.html)

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgement

Thanks to all partners of ForestGEO, for sharing their ideas and code.
