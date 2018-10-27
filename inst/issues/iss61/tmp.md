Compare rio and io
================

``` r
library(rio)
library(io)
```

    ## Loading required package: filenamer

``` r
directory <- "input/"
files <- fs::dir_ls(directory)
one_file <- files[[1]]
excel_file <- "input/excel/excel_file.xlsx"
a_df_list <- io::qread(directory)

# + io can deal with directories
io::qread(directory)
```

    ## $cars1
    ##   speed dist
    ## 1     4    2
    ## 2     4   10
    ## 3     7    4
    ## 4     7   22
    ## 5     8   16
    ## 6     9   10
    ## 
    ## $cars2
    ##   speed dist
    ## 1     4    2
    ## 2     4   10
    ## 3     7    4
    ## 4     7   22
    ## 5     8   16
    ## 6     9   10
    ## 
    ## $cars3
    ##   speed dist
    ## 1     4    2
    ## 2     4   10
    ## 3     7    4
    ## 4     7   22
    ## 5     8   16
    ## 6     9   10

``` r
# - rio can't deal with directories

# + rio can read files
import_list(files)
```

    ## Warning in FUN(X[[i]], ...): Import failed for input/excel

    ## $cars1
    ##   speed dist
    ## 1     4    2
    ## 2     4   10
    ## 3     7    4
    ## 4     7   22
    ## 5     8   16
    ## 6     9   10
    ## 
    ## $cars2
    ##   speed dist
    ## 1     4    2
    ## 2     4   10
    ## 3     7    4
    ## 4     7   22
    ## 5     8   16
    ## 6     9   10
    ## 
    ## $cars3
    ##   speed dist
    ## 1     4    2
    ## 2     4   10
    ## 3     7    4
    ## 4     7   22
    ## 5     8   16
    ## 6     9   10
    ## 
    ## $excel
    ## NULL

``` r
# - io can't read files 

# + io can read a single file
io::qread(one_file)
```

    ##   speed dist
    ## 1     4    2
    ## 2     4   10
    ## 3     7    4
    ## 4     7   22
    ## 5     8   16
    ## 6     9   10

``` r
# + rio can read a single file
rio::import(one_file)
```

    ##   speed dist
    ## 1     4    2
    ## 2     4   10
    ## 3     7    4
    ## 4     7   22
    ## 5     8   16
    ## 6     9   10

``` r
# + rio can read excel
rio::import(excel_file)
```

    ##   speed dist
    ## 1     4    2
    ## 2     4   10
    ## 3     7    4
    ## 4     7   22
    ## 5     8   16
    ## 6     9   10

``` r
# - io can read excel

# + rio can export lists to a single excell file
export(a_df_list, "output/multi_cars.xlsx")
# + io can't export lists to a single excel file

# + io can export lists to a multiple rds files
io::qwrite(a_df_list, "output/out.rds")
# - rio can't export lists to a multiple rds files
rio::export(a_df_list, "output/out.rds")
```

    ## Error in rio::export(a_df_list, "output/out.rds"): 'x' is not a data.frame or matrix

``` r
# + rio can export lists to a multiple rds files
rio::export(a_df_list, "output/out.rdata")
# + io can export lists to a multiple rds files
io::qwrite(a_df_list, "output/out.rdata")

# - io can't read Rdata
io::qread("output/out.rdata")
```

    ## Error in .qio_error(file, type): File type `rdata` is not supported

``` r
# - rio can read Rdata
head(rio::import("output/cars.RData"))
```

    ##   speed dist
    ## 1     4    2
    ## 2     4   10
    ## 3     7    4
    ## 4     7   22
    ## 5     8   16
    ## 6     9   10
