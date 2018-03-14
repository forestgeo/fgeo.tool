#' Replace literal 'NULL' (string) by `NA_character_` (missing value).
#'
#' Importing a dataset can be challenging. A common problem occurrs when reading
#' data a dataset with missing values encoded as 'NULL'. The problem is this:
#' When you read that dataset, R will interpret those missing values incorrectly
#' -- not as `NA` but as the literal string 'NULL'. This funciton sanitizes a
#' dataset by converting the literal string 'NULL' to the missing charecter
#' value `NA_character_`.
#'
#' Instead of using this function, you should avoid missinterpreting 'NULL' in
#' the first place. You can pass 'NULL' to the argument `na` of the functions
#' `read_*()` of the __readr__ package (e.g. read_csv()). For example:
#' ```R
#' readr::read_csv("your-file.csv", na = c("", "NA", "NULL"))
#' ```
#'
#' For more details on how to read data simply and safely see 
#' [this blog post](https://goo.gl/YDhgt6).
#'
#' @param x A dataframe or vector.
#'
#' @return A modified version of `x` with literals "NULL" replaced by
#'   `NA_character_`.
#' @export
#'
#' @examples
#' chr <- c("a", "NULL")
#' replace_null_by_na(chr)
#' 
#' dfm <- data.frame(
#'   a = c(1, 2, "NULL"), b = c(1, "NULL", 3), 
#'   stringsAsFactors = FALSE
#' )
#' dfm
#' replace_null_by_na(dfm)
replace_null_by_na <- function(x) {
  UseMethod("replace_null_by_na")
}

#' @export
replace_null_by_na.data.frame <- function(x) {
  purrr::map_df(x, replace_null_by_na.character)
}

#' @export
replace_null_by_na.character <- function(x) {
  ifelse(grepl("NULL", x), NA_character_, x)
}

#' @export
replace_null_by_na.logical <- function(x) replace_null_by_na.character(x)
#' @export
replace_null_by_na.integer <- function(x) replace_null_by_na.character(x)
#' @export
replace_null_by_na.double  <- function(x) replace_null_by_na.character(x)
#' @export
replace_null_by_na.default <- function(x) {
  stop(
    "`replace_null_by_na()` is not defined for objects of class ", 
    class(x), call. = FALSE
  )
}
