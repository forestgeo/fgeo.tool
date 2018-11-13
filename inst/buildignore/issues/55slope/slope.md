Slope
================
Mauro Lepore

``` r
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(tibble)
```

## Example data

``` r
elevation_ls <- fgeo.data::luquillo_elevation
str(elevation_ls)
## List of 4
##  $ col :Classes 'tbl_df', 'tbl' and 'data.frame':    6565 obs. of  3 variables:
##   ..$ x   : int [1:6565] 0 0 0 0 0 0 0 0 0 0 ...
##   ..$ y   : int [1:6565] 0 5 10 15 20 25 30 35 40 45 ...
##   ..$ elev: num [1:6565] 364 364 363 363 363 ...
##  $ mat : num [1:101, 1:65] 364 364 363 363 363 ...
##  $ xdim: int 320
##  $ ydim: int 500
```

## Paramenters

``` r
gridsize <- 20
plotdim <- c(320, 500)
edgecorrect <- TRUE
n_clusters <- 4
```

## Slope and convexity

``` r
meanelev_convex_slope <- ctfs::allquadratslopes(
  elevation_ls,
  gridsize = gridsize,
  plotdim = plotdim,
  edgecorrect = edgecorrect
)
## Calculating topographic indices for  400  quadrats
## 0   0
as_tibble(meanelev_convex_slope)
## # A tibble: 400 x 3
##    meanelev convex slope
##       <dbl>  <dbl> <dbl>
##  1     365. -0.390  8.87
##  2     365. -0.285 13.7 
##  3     367.  0.210 14.4 
##  4     367.  0.34  15.9 
##  5     362. -0.255 20.8 
##  6     358. -0.510 12.4 
##  7     359. -0.28  17.1 
##  8     360. -0.265 22.1 
##  9     362.  0.055 24.3 
## 10     364.  0.415 15.2 
## # ... with 390 more rows
```

We can create “habitats” by clustering `meanelev`, `convex` and `slope`.

``` r
meanelev_convex_slope <- mutate(
  meanelev_convex_slope, 
  habitats =  kmeans(meanelev_convex_slope, n_clusters)$cluster
)

habitat <- rowid_to_column(meanelev_convex_slope, "index")
as_tibble(habitat)
## # A tibble: 400 x 5
##    index meanelev convex slope habitats
##    <int>    <dbl>  <dbl> <dbl>    <int>
##  1     1     365. -0.390  8.87        1
##  2     2     365. -0.285 13.7         1
##  3     3     367.  0.210 14.4         1
##  4     4     367.  0.34  15.9         1
##  5     5     362. -0.255 20.8         1
##  6     6     358. -0.510 12.4         2
##  7     7     359. -0.28  17.1         2
##  8     8     360. -0.265 22.1         2
##  9     9     362.  0.055 24.3         1
## 10    10     364.  0.415 15.2         1
## # ... with 390 more rows
```

`ctfs::allquadratslopes()` didn’t output the `gx` and `gy` coordinates
of each row of the output, but the row indices correspond to quadrat
index.

``` r
# The names of the columns isn't quite right. I plan to make `add_index()` a bit
# more flexible: https://github.com/forestgeo/fgeo.tool/issues/52
elevation_df <- dplyr::rename(elevation_ls$col, gx = x, gy = y)

elevation_df <- fgeo.tool::add_index(elevation_df)
## Gessing: plotdim = c(320, 500)
## * If guess is wrong, provide the correct argument `plotdim`
elevation_df
## # A tibble: 6,565 x 4
##       gx    gy  elev index
##    <int> <int> <dbl> <dbl>
##  1     0     0  364.     1
##  2     0     5  364.     1
##  3     0    10  363.     1
##  4     0    15  363.     1
##  5     0    20  363      2
##  6     0    25  363.     2
##  7     0    30  363.     2
##  8     0    35  363.     2
##  9     0    40  363.     3
## 10     0    45  364.     3
## # ... with 6,555 more rows
```

Now we can use `index` to link `gx`, `gy` to `hatibats` created with
`ctfs::allquadratslopes()`.

``` r
round_gxgy_to_gridsize <- function(x, gridsize) {
  fgeo.base::check_crucial_names(x, c("gx", "gy"))
  
  x$gx <- fgeo.base::round_any(x$gx, gridsize)
  x$gy <- fgeo.base::round_any(x$gy, gridsize)
  x
}

gxgy_hab <- left_join(elevation_df, habitat)
## Joining, by = "index"
gxgy_hab <- unique(gxgy_hab[c("gx", "gy", "habitats")])
gxgy_hab <- round_gxgy_to_gridsize(gxgy_hab, gridsize = gridsize)
gxgy_hab
## # A tibble: 6,565 x 3
##       gx    gy habitats
##    <dbl> <dbl>    <int>
##  1     0     0        1
##  2     0     0        1
##  3     0     0        1
##  4     0    20        1
##  5     0    20        1
##  6     0    20        1
##  7     0    40        1
##  8     0    40        1
##  9     0    40        1
## 10     0    40        1
## # ... with 6,555 more rows
```

## Acknowledgments

Daniel Zuleta guided the development of this code, that I hope to use to
improve `fgeo_habitat()`.
