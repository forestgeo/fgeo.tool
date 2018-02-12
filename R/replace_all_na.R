#' Replace all NA's of a dataframe with a filler
#'
#' @param df A dataframe.
#' @param replace A filler; whatever you want to replace NA's with.
#'
#' @return A modified versio of `df`.
#' @export
#'
#' @examples
#' df <- data.frame(x = c(NA, 1), y = c("a", NA), stringsAsFactors = FALSE)
#' replace_all_na(df)
#' replace_all_na(df, "")
#' replace_all_na(df, "missing")
replace_all_na <- function(df, filler = 0) {
  stopifnot(is.data.frame(df))
  replace(df, is.na(df), filler)
}
