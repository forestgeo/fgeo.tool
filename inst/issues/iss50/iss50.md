`xdim` and `ydim` must be provided if using the elevation dataframe
================

``` r
library(fgeo)
## -- Attaching packages ------------------------------------------ fgeo 0.0.0.9000 --
## v fgeo.abundance  0.0.0.9004     v fgeo.demography 0.0.0.9000
## v fgeo.base       0.0.0.9001     v fgeo.habitat    0.0.0.9006
## v fgeo.data       0.0.0.9002     v fgeo.map        0.0.0.9204
## v fgeo.abundance  0.0.0.9004     v fgeo.tool       0.0.0.9003
## 

load(here::here("data-raw/private/CTFSElev_rabi.rdata"))

# If using the list, you don't need `xdim`, `ydim` because they come in the list
elev_list <- CTFSElev_rabi
str(elev_list)
## List of 4
##  $ col :'data.frame':    10201 obs. of  3 variables:
##   ..$ x   : num [1:10201] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ y   : num [1:10201] 0 5 10 15 20 25 30 35 40 45 ...
##   ..$ elev: num [1:10201] 55.5 56.1 56.3 56.5 56.5 ...
##  $ mat : num [1:101, 1:101] 55.5 56.1 56.3 56.5 56.5 ...
##  $ ydim: num 500
##  $ xdim: num 500
fgeo_habitat(elev_list, gridsize = 20, n = 3)
## # A tibble: 10,000 x 3
##       gx    gy habitats
##  * <dbl> <dbl>    <int>
##  1     0     0        3
##  2     0     5        3
##  3     0    10        3
##  4     0    15        3
##  5     0    20        3
##  6     0    25        3
##  7     0    30        3
##  8     0    35        3
##  9     0    40        3
## 10     0    45        3
## # ... with 9,990 more rows
```

``` r
# If using the dataframe, you must provide `xdim`, `ydim`

elev_dataframe <- elev_list$col
fgeo_habitat(elev_dataframe, gridsize = 20, n = 3, xdim = 500, ydim = 500)
## # A tibble: 10,000 x 3
##       gx    gy habitats
##  * <dbl> <dbl>    <int>
##  1     0     0        3
##  2     0     5        3
##  3     0    10        3
##  4     0    15        3
##  5     0    20        3
##  6     0    25        3
##  7     0    30        3
##  8     0    35        3
##  9     0    40        3
## 10     0    45        3
## # ... with 9,990 more rows
```

This fix adds a more informative message if you forget to pass `xdim`,
`ydim`.

``` r
fgeo_habitat(elev_dataframe, gridsize = 20, n = 3)
## Error: `xdim` and `ydim` can't be missing if `elevation` is a data.frame
```
