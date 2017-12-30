
<!-- README.md is generated from README.Rmd. Please edit that file -->
fgeo.utils: Utility functions of ForestGEO <img src="https://i.imgur.com/39pvr4n.png" align="right" height=44 />
================================================================================================================

[![Travis build status](https://travis-ci.org/forestgeo/fgeo.utils.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.utils) [![Coverage status](https://coveralls.io/repos/github/forestgeo/fgeo.utils/badge.svg)](https://coveralls.io/r/forestgeo/fgeo.utils?branch=master) [![CRAN status](http://www.r-pkg.org/badges/version/fgeo.utils)](https://cran.r-project.org/package=fgeo.utils)

Installation
------------

You can install fgeo.utils from github with:

    # install.packages("remotes")
    remotes::install_github("forestgeo/fgeo.utils")

``` r
library(fgeo.utils)
```

Example
-------

Setup.

``` r
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
add_status_tree(df)
#> # A tibble: 12 x 4
#>    CensusID   Tag Status status_tree
#>       <dbl> <dbl>  <chr>       <chr>
#>  1        1     1  alive       alive
#>  2        1     1   dead       alive
#>  3        1     2   dead        dead
#>  4        1     2   dead        dead
#>  5        1     3   dead        dead
#>  6        1     3   dead        dead
#>  7        2     1  alive       alive
#>  8        2     1  alive       alive
#>  9        2     2  alive       alive
#> 10        2     2   dead       alive
#> 11        2     3   dead        dead
#> 12        2     3   dead        dead

# Filter a data set

# Filter from the head or tail of a variable
top(df, Tag)
#> # A tibble: 4 x 3
#>   CensusID   Tag Status
#>      <dbl> <dbl>  <chr>
#> 1        1     1  alive
#> 2        1     1   dead
#> 3        2     1  alive
#> 4        2     1  alive
top(df, Tag, -1)
#> # A tibble: 4 x 3
#>   CensusID   Tag Status
#>      <dbl> <dbl>  <chr>
#> 1        1     3   dead
#> 2        1     3   dead
#> 3        2     3   dead
#> 4        2     3   dead
# Remove trees found dead in two or more censuses
rm_dead_twice(df)
#> # A tibble: 6 x 4
#>   CensusID   Tag Status status_tree
#>      <dbl> <dbl>  <chr>       <chr>
#> 1        1     1  alive       alive
#> 2        1     1   dead       alive
#> 3        2     1  alive       alive
#> 4        2     1  alive       alive
#> 5        2     2  alive       alive
#> 6        2     2   dead       alive
```

Check inputs.

``` r
# Silent means success
check_crucial_names(df, "Status")

# Errs if the data hasn't a given name
check_crucial_names(df, "DBH")
#> Error: Ensure your data set has these variables:
#> DBH

check_unique(df, "CensusID", msg = "* Is this what you expect?")
#> Warning in do.call(cond, list(customized)): Duplicated values were detected
#> * Is this what you expect?
```

Much you can do directly with dplyr.

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
#> # A tibble: 3 x 3
#>   CensusID   Tag Status
#>      <dbl> <dbl>  <chr>
#> 1        2     1  alive
#> 2        2     1  alive
#> 3        2     2  alive
```

You can combine **fgeo.utils** with **dplyr**.

``` r
edited <- add_status_tree(top(df, CensusID, -1))
dplyr::select(edited, -Status)
#> # A tibble: 6 x 3
#>   CensusID   Tag status_tree
#>      <dbl> <dbl>       <chr>
#> 1        2     1       alive
#> 2        2     1       alive
#> 3        2     2       alive
#> 4        2     2       alive
#> 5        2     3        dead
#> 6        2     3        dead
```

You don't have to, but if you want you can use the pipe (`%>%`).

``` r
# With the pipe
df %>% 
  add_status_tree() %>%
  dplyr::filter(status_tree == "alive") %>%
  dplyr::rename(status_stem = Status) %>%
  dplyr::arrange(desc(CensusID))
#> # A tibble: 6 x 4
#>   CensusID   Tag status_stem status_tree
#>      <dbl> <dbl>       <chr>       <chr>
#> 1        2     1       alive       alive
#> 2        2     1       alive       alive
#> 3        2     2       alive       alive
#> 4        2     2        dead       alive
#> 5        1     1       alive       alive
#> 6        1     1        dead       alive

# Same but without the pipe: It is hard to understand what is going on.
dplyr::arrange(
  dplyr::rename(
    dplyr::filter(
      add_status_tree(df), status_tree == "alive"), 
    status_stem = Status
  ), 
  desc(CensusID)
)
#> # A tibble: 6 x 4
#>   CensusID   Tag status_stem status_tree
#>      <dbl> <dbl>       <chr>       <chr>
#> 1        2     1       alive       alive
#> 2        2     1       alive       alive
#> 3        2     2       alive       alive
#> 4        2     2        dead       alive
#> 5        1     1       alive       alive
#> 6        1     1        dead       alive
```
