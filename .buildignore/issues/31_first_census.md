Process and explore FastField data of a first census
================

``` r
# https://forestgeo.github.io/fgeo.tool/#installation
library(fgeo.tool)

# Install with `install.packages("pkg")`
library(fs)
library(tidyverse)
#> -- Attaching packages --------------------------------------------- tidyverse 1.2.1 --
#> v ggplot2 2.2.1     v purrr   0.2.4
#> v tibble  1.4.2     v dplyr   0.7.4
#> v tidyr   0.8.0     v stringr 1.3.1
#> v readr   1.1.1     v forcats 0.3.0
#> -- Conflicts ------------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
```

Change your input and output directory before knitting this file, and
re-run every day you come back from the field.

![](https://i.imgur.com/fGOtlVz.png)

![](https://i.imgur.com/C2SbGMb.png)

``` r
# My input directory
params$input_dir
#> [1] "C:/Users/LeporeM/Documents/R/R-3.5.0/library/fgeo.tool/extdata/first_census"
dir(params$input_dir, "xlsx")
#> [1] "census.xlsx"

# My output directory (temporary)
params$output_dir
#> [1] "C:\\Users\\LeporeM\\AppData\\Local\\Temp\\1\\RtmpYr5gZl"
```

``` r
xl_sheets_to_csv(params$input_dir, params$output_dir, first_census = TRUE)
#> Warning: `.x` has no dataframe new_secondary_stems
#> Warning: `.x` has no dataframe recruits
# Output is one .csv per workbook
dir(params$output_dir, "csv")
#> [1] "census.csv"
```

``` r
# Now input multiple .csv and output a single dataframe
combo <- csv_to_df(params$output_dir)
#> Parsed with column specification:
#> cols(
#>   submission_id = col_character(),
#>   quadrat = col_character(),
#>   tag = col_character(),
#>   stem_tag = col_integer(),
#>   species = col_character(),
#>   species_code = col_character(),
#>   dbh = col_double(),
#>   status = col_character(),
#>   codes = col_character(),
#>   notes = col_character(),
#>   pom = col_double(),
#>   sheet = col_character(),
#>   section_id = col_integer(),
#>   unique_stem = col_character(),
#>   date = col_character()
#> )
combo
#> # A tibble: 49 x 15
#>    submission_id  quadrat tag   stem_tag species species_code   dbh status
#>    <chr>          <chr>   <chr>    <int> <chr>   <chr>        <dbl> <chr> 
#>  1 c01b37d5-2c30~ 0816    007          1 Americ~ TILIAM        51.2 LI    
#>  2 efd8e991-2e34~ 0916    004          1 Americ~ TILIAM        31.6 LI    
#>  3 efd8e991-2e34~ 0916    007          1 Ironwo~ OSTRVI        10.2 LI    
#>  4 efd8e991-2e34~ 0916    008          1 Ironwo~ OSTRVI         2.7 LI    
#>  5 efd8e991-2e34~ 0916    010          1 Hackbe~ CELTOC        25.9 LI    
#>  6 efd8e991-2e34~ 0916    012          1 Bur Oak QUERMA        35.2 LI    
#>  7 7997596a-c94e~ 1017    001          1 Americ~ TILIAM        65.3 LI    
#>  8 572ddae1-d02c~ 0816    002          1 Hackbe~ CELTOC        45.1 LI    
#>  9 c01b37d5-2c30~ 0816    007          2 TILIAM  <NA>          42.4 LI    
#> 10 c01b37d5-2c30~ 0816    007          3 TILIAM  <NA>          39.8 LI    
#> # ... with 39 more rows, and 7 more variables: codes <chr>, notes <chr>,
#> #   pom <dbl>, sheet <chr>, section_id <int>, unique_stem <chr>,
#> #   date <chr>
```

``` r
# Read data of tree positions
where_dir <- example_path("first_census/position.csv")
where <- read_csv(where_dir)
#> Parsed with column specification:
#> cols(
#>   PtID = col_character(),
#>   East = col_double(),
#>   North = col_double()
#> )

# Compare
where
#> # A tibble: 23 x 3
#>    PtID     East North
#>    <chr>   <dbl> <dbl>
#>  1 0816006  150.  312.
#>  2 0816007  153.  301.
#>  3 0816008  143.  303.
#>  4 0816009  147.  302.
#>  5 0916001  170.  317.
#>  6 0916002  170.  314.
#>  7 0916003  168.  310.
#>  8 0916003  168.  310.
#>  9 0916004  165.  317.
#> 10 0916005  165.  312.
#> # ... with 13 more rows
select(combo, quadrat, tag)
#> # A tibble: 49 x 2
#>    quadrat tag  
#>    <chr>   <chr>
#>  1 0816    007  
#>  2 0916    004  
#>  3 0916    007  
#>  4 0916    008  
#>  5 0916    010  
#>  6 0916    012  
#>  7 1017    001  
#>  8 0816    002  
#>  9 0816    007  
#> 10 0816    007  
#> # ... with 39 more rows
```

``` r
# Create a variable that we can later use to merge the two datasets
combo <- mutate(combo, PtID = paste0(quadrat, tag))
combo2 <- left_join(combo, where)
#> Joining, by = "PtID"
# Reorganize columns for easier visualization
combo2 <- select(combo2, PtID, East, North, date, everything())
combo2
#> # A tibble: 50 x 18
#>    PtID    East North date  submission_id  quadrat tag   stem_tag species 
#>    <chr>  <dbl> <dbl> <chr> <chr>          <chr>   <chr>    <int> <chr>   
#>  1 08160~  153.  301. 2018~ c01b37d5-2c30~ 0816    007          1 America~
#>  2 09160~  165.  317. 2018~ efd8e991-2e34~ 0916    004          1 America~
#>  3 09160~  163.  309. 2018~ efd8e991-2e34~ 0916    007          1 Ironwood
#>  4 09160~  162.  305. 2018~ efd8e991-2e34~ 0916    008          1 Ironwood
#>  5 09160~  165.  303. 2018~ efd8e991-2e34~ 0916    010          1 Hackber~
#>  6 09160~  179.  304. 2018~ efd8e991-2e34~ 0916    012          1 Bur Oak 
#>  7 10170~  198.  325. 2018~ 7997596a-c94e~ 1017    001          1 America~
#>  8 08160~   NA    NA  2018~ 572ddae1-d02c~ 0816    002          1 Hackber~
#>  9 08160~  153.  301. 2018~ c01b37d5-2c30~ 0816    007          2 TILIAM  
#> 10 08160~  153.  301. 2018~ c01b37d5-2c30~ 0816    007          3 TILIAM  
#> # ... with 40 more rows, and 9 more variables: species_code <chr>,
#> #   dbh <dbl>, status <chr>, codes <chr>, notes <chr>, pom <dbl>,
#> #   sheet <chr>, section_id <int>, unique_stem <chr>
```

``` r
# Helpers to avoid duplication
set_brk <- function(max, by) seq(0, max, by)
set_lim <- function(max) c(0, max)

xmax <- 560
ymax <- 360
ggplot(combo2, aes(East, North, color = date)) + 
  geom_point() +
  coord_equal() +
  scale_x_continuous(minor_breaks = set_brk(xmax, 20), limits = set_lim(xmax)) +
  scale_y_continuous(minor_breaks = set_brk(ymax, 20), limits = set_lim(ymax)) +
  theme_bw()
#> Warning: Removed 8 rows containing missing values (geom_point).
```

<img src="31_first_census_files/figure-gfm/unnamed-chunk-7-1.png" width="95%" style="display: block; margin: auto;" />

**ggplot2** removes missing values from `East` and `North` but there may
be missing values in `date`.

``` r
filter(combo2, is.na(date))
#> # A tibble: 2 x 18
#>   PtID    East North date  submission_id    quadrat tag   stem_tag species
#>   <chr>  <dbl> <dbl> <chr> <chr>            <chr>   <chr>    <int> <chr>  
#> 1 NA001    NA    NA  <NA>  a4e952e6-dcad-4~ <NA>    001          1 Bur Oak
#> 2 08180~  155.  353. <NA>  a4e952e6-dcad-4~ 0818    002          1 Bur Oak
#> # ... with 9 more variables: species_code <chr>, dbh <dbl>, status <chr>,
#> #   codes <chr>, notes <chr>, pom <dbl>, sheet <chr>, section_id <int>,
#> #   unique_stem <chr>
```
