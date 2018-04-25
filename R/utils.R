commas <- function(...) {
  paste0(..., collapse = ", ")
}

max0 <- function(...) {
  max(..., na.rm = TRUE)
}

min0 <- function(...) {
  min(..., na.rm = TRUE)
}



# Tests -------------------------------------------------------------------

has_class_df <- function(x) {
  any(grepl("data.frame", class(x)))
}

each_list_item_is_df <- function(x) {
  if (!is.list(x) || is.data.frame(x)) {
    abort("`x` must be a list of datafraems (and not itself a dataframe).")
  }
  all(purrr::map_lgl(x, has_class_df))
}



# plyr --------------------------------------------------------------------

#' Round to multiple of any number.
#' 
#' Copied from `plyr:::round_any.numeric()`.
#' 
#' @param x Numeric vector to round.
#' @param accuracy Number to round to.
#' @param f Rounding function: floor, ceiling or round.
#' 
#' @seealso `plyr::round_any()` and \url{http://bit.ly/2JrBQK3}.
#' 
#' @noRd
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}
