#' Detect and flag based on a predicate applyed to a variable by groups.
#' 
#' These functions extend [fgeo.base::flag_if()] and [fgeo.base::detect_if()] to
#' work by groups defined with [dplyr::group_by()].
#'
#' @param .data A dataframe.
#' @param name String. The name of a column of the dataframe.
#' @param predicate A predicate function, e.g. [fgeo.base::is_multiple()].
#' @param condition A condition function, e.g. [rlang::inform()] or
#'   [base::stop()].
#' @param msg String. An optional custom-messsage.
#'
#' @family functions to check inputs.
#' @family functions for developers.
#' @family predicates.
#'
#' @return
#' * `flag_if_group()`: A condition and its first input, invisibly.
#' * `detect_if_group()`: Logical of length 1.
#' @export
#'
#' @examples
#' tree <- tibble(CensusID = c(1, 2), treeID = c(1, 2))
#' tree
#' by_censusid <- group_by(tree, CensusID)
#' expect_silent(flag_if_group(by_censusid, "treeID", is_multiple))
#' expect_warning(flag_if_group(tree, "treeID", is_multiple), msg)
flag_if_group <- function(.data, 
  name, 
  predicate, 
  condition = warn, 
  msg = NULL) {
  stopifnot(length(condition) == 1)
  
  result_by_groups <- by_group(.data, function(x) detect_if(x, name, predicate))
  detected <- any(t(result_by_groups))
  if (detected) condition(msg %||% glue("{name}: Flagged values were detected."))
  
  invisible(.data)
}

detect_if_group <- function(.data, name, predicate) {
  any(t(by_group(.data, function(x) detect_if(x, name, is_duplicated))))
}
