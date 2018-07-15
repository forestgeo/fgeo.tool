Read, combine, identify, and write multiple files
================
Mauro Lepore
2018-07-15

``` r
library(fgeo.base)
library(fgeo.tool)
#> 
#> Attaching package: 'fgeo.tool'
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(fs)
library(purrr)
#> 
#> Attaching package: 'purrr'
#> The following object is masked from 'package:fgeo.base':
#> 
#>     %||%
library(readr)
library(writexl)
```

If you have multiple files in a directory, you may want to combine them
all into a single dataframe and export it as a .csv or excel file. Here
is how you can do this and a a little more.

Suppose you have these files in a directory:

``` r
files <- tool_example("files")
dir(files)
#> [1] "01.csv"  "01.xls"  "02.csv"  "02.xlsx"
```

Let’s read all the .csv files into a single dataframe.

``` r
combined <- csv_to_df(files)
#> Parsed with column specification:
#> cols(
#>   x = col_integer(),
#>   y = col_character()
#> )
#> Parsed with column specification:
#> cols(
#>   x = col_integer(),
#>   y = col_character()
#> )
combined
#> # A tibble: 10 x 2
#>        x y    
#>    <int> <chr>
#>  1     1 a    
#>  2     2 b    
#>  3     3 c    
#>  4     4 d    
#>  5     5 e    
#>  6     1 k    
#>  7     2 l    
#>  8     3 m    
#>  9     4 n    
#> 10     5 o
```

You can then write a .csv file.

``` r
write_csv(combined, "combined.csv")
```

An alternative is to read each file into an individual dataframe and
store all dataframes into a list.

``` r
dfs <- xl_to_dfs(files)
dfs
#> $`01.xls`
#> # A tibble: 5 x 2
#>       x y    
#>   <dbl> <chr>
#> 1     1 a    
#> 2     2 b    
#> 3     3 c    
#> 4     4 d    
#> 5     5 e    
#> 
#> $`02.xlsx`
#> # A tibble: 5 x 2
#>       x y    
#>   <dbl> <chr>
#> 1     1 k    
#> 2     2 l    
#> 3     3 m    
#> 4     4 n    
#> 5     5 o
```

This intermediate step is useful if you want to identify the source of
each dataframe.

``` r
id <- name_dfs(dfs, name = "source")
id
#> $`01.xls`
#>   x y source
#> 1 1 a 01.xls
#> 2 2 b 01.xls
#> 3 3 c 01.xls
#> 4 4 d 01.xls
#> 5 5 e 01.xls
#> 
#> $`02.xlsx`
#>   x y  source
#> 1 1 k 02.xlsx
#> 2 2 l 02.xlsx
#> 3 3 m 02.xlsx
#> 4 4 n 02.xlsx
#> 5 5 o 02.xlsx
```

You can then reduce the structure of these data by row-binding each
dataframe of the list into a single dataframe.

``` r
combined_id <- reduce(id, rbind)
combined_id
#>    x y  source
#> 1  1 a  01.xls
#> 2  2 b  01.xls
#> 3  3 c  01.xls
#> 4  4 d  01.xls
#> 5  5 e  01.xls
#> 6  1 k 02.xlsx
#> 7  2 l 02.xlsx
#> 8  3 m 02.xlsx
#> 9  4 n 02.xlsx
#> 10 5 o 02.xlsx
```

Now you can save this dataframe to a .csv file exactly as you did
before.

``` r
write_csv(combined_id, "combined_id.csv")
```

Instead of a .csv you may write an excel file of the combined dataframe.

``` r
write_xlsx(combined_id, "combined_id.xlsx")
```

Or you may map each dataframe of the list to an individual sheet of a
single excel workbook.

``` r
write_xlsx(id, "combined_id.xlsx")
```

<img src="https://i.imgur.com/00TSP2C.png" align="center" height=250 />

If later you need to read multiple excel files you may use the same
approach you used to read .csv files with the functions `xl_to_dfs()`
and `xl_to_df()`. For more details see `?files_to_df`.

### Reading data safely

  - [With **readr** and
    RStudio](https://fgeo.netlify.com/2018/03/13/2018-03-13-import-dataset/)
    (recommended).
  - [With only base
    R](https://fgeo.netlify.com/2018/03/15/2018-03-15-how-to-read-data-safely-with-only-base-r/).

### Acknowledgment

Thanks to Jessica Shue for inspiring the code and workflow shown here.
