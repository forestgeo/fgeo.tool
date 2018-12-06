#' Apply a predicate function to a column of a dataframe.
#'
#' @param .data A dataframe.
#' @inheritParams flag_if
#'
#' @family general predicates
#'
#' @return Logical of length 1.
#' @keywords internal
#' @noRd
#'
#' @examples
#' dfm <- data.frame(CensusID = c(1, 2, NA))
#' detect_if(dfm, "censusid", is_multiple)
#' detect_if(dfm, "censusid", is_duplicated)
#'
#' dfm <- data.frame(CensusID = c(1, 1))
#' detect_if(dfm, "censusid", is_duplicated)
#' detect_if(dfm, "censusid", is_multiple)
#'
#' dfm <- data.frame(CensusID = c(1, 1, 2))
#' detect_if(dfm, "censusid", is_duplicated)
#' detect_if(dfm, "censusid", is_multiple)
detect_if <- function(.data, name, predicate) {
  name <- tolower(name)
  predicate(extract_column(.data, name))
}
