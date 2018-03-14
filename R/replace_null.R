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
#' @param replacement A value to replace 'NULL' with.
#'
#' @return A modified version of `x` with literals "NULL" replaced by
#'   `NA_character_`.
#' @export
#'
#' @examples
#' chr <- c("a", "NULL")
#' replace_null(chr)
#' replace_null(chr, "my_replacement")
#' 
#' dfm <- data.frame(
#'   a = c(1, 2, "NULL"), 
#'   b = c(1, "NULL", 3)
#' )
#' replace_null(dfm)
#' 
#' replace_null(dfm, -9999)
replace_null <- function(x,  replacement = NA_character_) {
  UseMethod("replace_null")
}

#' @export
replace_null.data.frame <- function(x, replacement = NA_character_) {
  if (any(purrr::map_lgl(x, is.factor))) {
    warning("Converting factors to strings.", call. = FALSE)
  }
  x <- purrr::modify_if(x, is.factor, as.character)
  purrr::map_df(x, replace_null.character, replacement = replacement)
}

#' @export
replace_null.character <- function(x, replacement = NA_character_) {
  if (is.null(replacement)) {
    stop("`replacement` must be not `NULL`", call. = FALSE)
  }
  ifelse(grepl("NULL", x), replacement, x)
}

#' @export
replace_null.factor <- function(x, replacement = NA_character_) {
  if (any(purrr::map_lgl(x, is.factor))) {
    warning("Converting factors to strings.", call. = FALSE)
  }
  x <- purrr::map_chr(x, ~as.character(.x[is.factor(.x)]))

  replace_null.character(x, replacement = replacement)
}

#' @export
replace_null.default <- function(x,  replacement = NA_character_) {
  defined_class <- c("logical", "integer", "numeric", "factor")
  has_defined_class <- any(class(x) %in% defined_class)
  if (has_defined_class) {
    replace_null.character(x, replacement = replacement)
  } else {
    stop(
      "`replace_null()` is not defined for objects of class ", 
      class(x), call. = FALSE
    )
  }
}
