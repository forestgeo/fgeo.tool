#' Replace all NA's of a dataframe with a filler
#'
#' @param x A dataframe.
#' @param filler A filler; whatever you want to replace NA's with.
#'
#' @return A modified versio of `x`.
#' 
#' @section Acknowledgements:
#'   This function was inspired by David Kenfack.
#' @export
#'
#' @examples
#' x <- data.frame(x = c(NA, 1), y = c("a", NA), stringsAsFactors = FALSE)
#' replace_all_na(x)
#' replace_all_na(x, "")
#' replace_all_na(x, "missing")
replace_all_na <- function(x, filler = 0) {
  stopifnot(is.data.frame(x))
  replace(x, is.na(x), filler)
}
