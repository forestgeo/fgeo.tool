
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

    # install.packages("remotes")
    remotes::install_github("forestgeo/fgeo.tool")

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

Setup.

``` r
library(fgeo.tool)

df <- tibble::tribble(
  ~CensusID, ~Tag, ~Status,
          1,    1, "alive",
          1,    1,  "dead",
                           
          1,    2,  "dead",
          1,    2,  "dead",
                           
          1,    3,  "dead",
          1,    3,  "dead",
                           
          2,    1, "alive",
          2,    1, "alive",
                           
          2,    2, "alive",
          2,    2,  "dead",
                           
          2,    3,  "dead",
          2,    3,  "dead"
)
```

Manipulate data.

``` r
# Mutate a data set

# Determine the status of each tree based on the status of its stems
df <- add_status_tree(df)
#> Warning: No observation has .status = D, A
#>   * Detected values: alive, dead

# Filter a data set

# Filter from the head or tail of a variable
pick_top(df, Tag)
#> # A tibble: 4 x 4
#>   CensusID   Tag Status status_tree
#>      <dbl> <dbl> <chr>  <chr>      
#> 1        1     1 alive  A          
#> 2        1     1 dead   A          
#> 3        2     1 alive  A          
#> 4        2     1 alive  A
pick_top(df, Tag, -1)
#> # A tibble: 4 x 4
#>   CensusID   Tag Status status_tree
#>      <dbl> <dbl> <chr>  <chr>      
#> 1        1     3 dead   A          
#> 2        1     3 dead   A          
#> 3        2     3 dead   A          
#> 4        2     3 dead   A
# Remove trees found dead in two or more censuses
drop_twice_dead(df)
#> # A tibble: 12 x 4
#>    CensusID   Tag Status status_tree
#>       <dbl> <dbl> <chr>  <chr>      
#>  1        1     1 alive  A          
#>  2        1     1 dead   A          
#>  3        1     2 dead   A          
#>  4        1     2 dead   A          
#>  5        1     3 dead   A          
#>  6        1     3 dead   A          
#>  7        2     1 alive  A          
#>  8        2     1 alive  A          
#>  9        2     2 alive  A          
#> 10        2     2 dead   A          
#> 11        2     3 dead   A          
#> 12        2     3 dead   A
```

Much you can do directly with **dplyr**.

``` r
# Using notation dplyr::fun to make it obvious where fun comes from
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

dplyr::filter(
  .data = df,
  CensusID > 1,
  Tag  %in% c(1, 2),
  Status == "alive"
)
#> # A tibble: 3 x 4
#>   CensusID   Tag Status status_tree
#>      <dbl> <dbl> <chr>  <chr>      
#> 1        2     1 alive  A          
#> 2        2     1 alive  A          
#> 3        2     2 alive  A
```

You can combine **fgeo.tool** with **dplyr**.

``` r
edited <- add_status_tree(pick_top(df, CensusID, -1))
#> Warning: No observation has .status = D, A
#>   * Detected values: alive, dead
dplyr::select(edited, -Status)
#> # A tibble: 6 x 3
#>   CensusID   Tag status_tree
#>      <dbl> <dbl> <chr>      
#> 1        2     1 A          
#> 2        2     1 A          
#> 3        2     2 A          
#> 4        2     2 A          
#> 5        2     3 A          
#> 6        2     3 A
```

You don’t have to, but if you want you can use the pipe (`%>%`).

``` r
# With the pipe
df %>% 
  add_status_tree() %>%
  dplyr::filter(status_tree == "alive") %>%
  dplyr::rename(status_stem = Status) %>%
  dplyr::arrange(desc(CensusID))
#> Warning: No observation has .status = D, A
#>   * Detected values: alive, dead
#> # A tibble: 0 x 4
#> # ... with 4 variables: CensusID <dbl>, Tag <dbl>, status_stem <chr>,
#> #   status_tree <chr>

# Same but without the pipe: It is hard to understand what is going on.
dplyr::arrange(
  dplyr::rename(
    dplyr::filter(
      add_status_tree(df), status_tree == "alive"), 
    status_stem = Status
  ), 
  desc(CensusID)
)
#> Warning: No observation has .status = D, A
#>   * Detected values: alive, dead
#> # A tibble: 0 x 4
#> # ... with 4 variables: CensusID <dbl>, Tag <dbl>, status_stem <chr>,
#> #   status_tree <chr>
```

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgements

Thanks to all partners of ForestGEO, for sharing their ideas and code.
