
<!-- README.md is generated from README.Rmd. Please edit that file -->
fgeo.tool: Utility functions of ForestGEO <img src="https://i.imgur.com/39pvr4n.png" align="right" height=44 />
===============================================================================================================

[![Travis build status](https://travis-ci.org/forestgeo/fgeo.tool.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.tool) [![Coverage status](https://coveralls.io/repos/github/forestgeo/fgeo.tool/badge.svg)](https://coveralls.io/r/forestgeo/fgeo.tool?branch=master) [![CRAN status](http://www.r-pkg.org/badges/version/fgeo.tool)](https://cran.r-project.org/package=fgeo.tool)

Installation
------------

You can install fgeo.tool from github with:

    # install.packages("remotes")
    remotes::install_github("forestgeo/fgeo.tool")

``` r
library(fgeo.tool)
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
df <- add_status_tree(df)
#> Warning in check_valid_status(x, .status = c(status_d, status_a), "status"): No observation has .status = D, A
#>   * Valid values: alive, dead

# Filter a data set

# Filter from the head or tail of a variable
top(df, Tag)
#> # A tibble: 4 x 4
#>   CensusID   Tag Status status_tree
#>      <dbl> <dbl> <chr>  <chr>      
#> 1     1.00  1.00 alive  A          
#> 2     1.00  1.00 dead   A          
#> 3     2.00  1.00 alive  A          
#> 4     2.00  1.00 alive  A
top(df, Tag, -1)
#> # A tibble: 4 x 4
#>   CensusID   Tag Status status_tree
#>      <dbl> <dbl> <chr>  <chr>      
#> 1     1.00  3.00 dead   A          
#> 2     1.00  3.00 dead   A          
#> 3     2.00  3.00 dead   A          
#> 4     2.00  3.00 dead   A
# Remove trees found dead in two or more censuses
discard_dead_twice(df)
#> # A tibble: 12 x 4
#>    CensusID   Tag Status status_tree
#>       <dbl> <dbl> <chr>  <chr>      
#>  1     1.00  1.00 alive  A          
#>  2     1.00  1.00 dead   A          
#>  3     1.00  2.00 dead   A          
#>  4     1.00  2.00 dead   A          
#>  5     1.00  3.00 dead   A          
#>  6     1.00  3.00 dead   A          
#>  7     2.00  1.00 alive  A          
#>  8     2.00  1.00 alive  A          
#>  9     2.00  2.00 alive  A          
#> 10     2.00  2.00 dead   A          
#> 11     2.00  3.00 dead   A          
#> 12     2.00  3.00 dead   A
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
#> # A tibble: 3 x 4
#>   CensusID   Tag Status status_tree
#>      <dbl> <dbl> <chr>  <chr>      
#> 1     2.00  1.00 alive  A          
#> 2     2.00  1.00 alive  A          
#> 3     2.00  2.00 alive  A
```

You can combine **fgeo.tool** with **dplyr**.

``` r
edited <- add_status_tree(top(df, CensusID, -1))
#> Warning in check_valid_status(x, .status = c(status_d, status_a), "status"): No observation has .status = D, A
#>   * Valid values: alive, dead
dplyr::select(edited, -Status)
#> # A tibble: 6 x 3
#>   CensusID   Tag status_tree
#>      <dbl> <dbl> <chr>      
#> 1     2.00  1.00 A          
#> 2     2.00  1.00 A          
#> 3     2.00  2.00 A          
#> 4     2.00  2.00 A          
#> 5     2.00  3.00 A          
#> 6     2.00  3.00 A
```

You don't have to, but if you want you can use the pipe (`%>%`).

``` r
# With the pipe
df %>% 
  add_status_tree() %>%
  dplyr::filter(status_tree == "alive") %>%
  dplyr::rename(status_stem = Status) %>%
  dplyr::arrange(desc(CensusID))
#> Warning in check_valid_status(x, .status = c(status_d, status_a), "status"): No observation has .status = D, A
#>   * Valid values: alive, dead
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
#> Warning in check_valid_status(x, .status = c(status_d, status_a), "status"): No observation has .status = D, A
#>   * Valid values: alive, dead
#> # A tibble: 0 x 4
#> # ... with 4 variables: CensusID <dbl>, Tag <dbl>, status_stem <chr>,
#> #   status_tree <chr>
```
