
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

Install the development version of **fgeo.tool**:

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
#> # A tibble: 3 x 19
#>   treeID stemID tag   StemTag sp    quadrat    gx    gy MeasureID CensusID
#>    <int>  <int> <chr> <chr>   <chr> <chr>   <dbl> <dbl>     <int>    <int>
#> 1    104    143 10009 10009   DACE~ 113      10.3  245.    439947        5
#> 2    119    158 1001~ 100104  MYRS~ 1021    183.   410.    466597        5
#> 3    180    225 1001~ 100174  CASA~ 921     165.   410.    466623        5
#> # ... with 9 more variables: dbh <dbl>, pom <chr>, hom <dbl>,
#> #   ExactDate <date>, DFstatus <chr>, codes <chr>, nostems <dbl>,
#> #   status <chr>, date <dbl>
#> 
#> $tree6
#> # A tibble: 3 x 19
#>   treeID stemID tag   StemTag sp    quadrat    gx    gy MeasureID CensusID
#>    <int>  <int> <chr> <chr>   <chr> <chr>   <dbl> <dbl>     <int>    <int>
#> 1    104    143 10009 10009   DACE~ 113      10.3  245.    582850        6
#> 2    119    158 1001~ 100104  MYRS~ 1021    183.   410.    578696        6
#> 3    180    225 1001~ 100174  CASA~ 921     165.   410.    617049        6
#> # ... with 9 more variables: dbh <dbl>, pom <chr>, hom <dbl>,
#> #   ExactDate <date>, DFstatus <chr>, codes <chr>, nostems <dbl>,
#> #   status <chr>, date <dbl>
```

Collapse all censuses into a single dataframe.

``` r
census56_df <- list_df(census56_list)
census56_df
#> # A tibble: 6 x 19
#>   treeID stemID tag   StemTag sp    quadrat    gx    gy MeasureID CensusID
#>    <int>  <int> <chr> <chr>   <chr> <chr>   <dbl> <dbl>     <int>    <int>
#> 1    104    143 10009 10009   DACE~ 113      10.3  245.    439947        5
#> 2    119    158 1001~ 100104  MYRS~ 1021    183.   410.    466597        5
#> 3    180    225 1001~ 100174  CASA~ 921     165.   410.    466623        5
#> 4    104    143 10009 10009   DACE~ 113      10.3  245.    582850        6
#> 5    119    158 1001~ 100104  MYRS~ 1021    183.   410.    578696        6
#> 6    180    225 1001~ 100174  CASA~ 921     165.   410.    617049        6
#> # ... with 9 more variables: dbh <dbl>, pom <chr>, hom <dbl>,
#> #   ExactDate <date>, DFstatus <chr>, codes <chr>, nostems <dbl>,
#> #   status <chr>, date <dbl>
```

Summarize by groups.

``` r
by_census <- group_by(census56_df, CensusID)
summarize(by_census, n = n_distinct(treeID))
#> Warning in summarise_impl(.data, dots): hybrid evaluation forced for
#> `n_distinct`. Please use dplyr::n_distinct() or library(dplyr) to remove
#> this warning.
#> # A tibble: 2 x 2
#>   CensusID     n
#>      <int> <int>
#> 1        5     3
#> 2        6     3

# Same
census56_df %>% 
  group_by(CensusID) %>% 
  summarize(n = n_distinct(treeID))
#> Warning in summarise_impl(.data, dots): hybrid evaluation forced for
#> `n_distinct`. Please use dplyr::n_distinct() or library(dplyr) to remove
#> this warning.
#> # A tibble: 2 x 2
#>   CensusID     n
#>      <int> <int>
#> 1        5     3
#> 2        6     3
```

Pick rows and reorganize columns.

``` r
census56_df %>% 
  pick_dbh_over(250) %>% 
  pick_status("A") %>%
  add_status_tree() %>% 
  select(dbh, contains("status"), everything())
#> Warning: No observation has .status = D, A
#>   * Detected values:
#> # A tibble: 0 x 20
#> # ... with 20 variables: dbh <dbl>, DFstatus <chr>, status <chr>,
#> #   status_tree <chr>, treeID <int>, stemID <int>, tag <chr>,
#> #   StemTag <chr>, sp <chr>, quadrat <chr>, gx <dbl>, gy <dbl>,
#> #   MeasureID <int>, CensusID <int>, pom <chr>, hom <dbl>,
#> #   ExactDate <date>, codes <chr>, nostems <dbl>, date <dbl>
```

[Get started with
**fgeo**](https://forestgeo.github.io/fgeo/articles/fgeo.html)

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
