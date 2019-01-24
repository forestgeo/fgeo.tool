#' Apply a predicate function to a column of a dataframe.
#'
#' @inheritParams flag_if
#' @param .data A dataframe.
#'
#' @return Logical of length 1.
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
#' @keywords internal
#' @family general predicates
#' @noRd
detect_if <- function(.data, name, predicate) {
  name <- tolower(name)
  predicate(extract_column(.data, name))
}
