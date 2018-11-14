#' Detect and flag based on a predicate applied to a variable by groups.
#' 
#' These functions extend [fgeo.base::flag_if()] and [fgeo.base::detect_if()] to
#' work by groups defined with [dplyr::group_by()].
#'
#' @param .data A dataframe.
#' @param name String. The name of a column of the dataframe.
#' @param predicate A predicate function, e.g. [fgeo.base::is_multiple()].
#' @param condition A condition function, e.g. [rlang::inform()] or
#'   [base::stop()].
#' @param msg String to customize the returned message.
#'
#' @family functions to check inputs.
#' @family predicates.
#' @family functions for developers.
#' @keywords internal
#'
#' @return
#' * `flag_if_group()`: A condition and its first input, invisibly.
#' * `detect_if_group()`: Logical of length 1.
#' @export
#'
#' @examples
#' library(fgeo.base)
#' 
#' tree <- tibble(CensusID = c(1, 2), treeID = c(1, 2))
#' detect_if_group(tree, "treeID", is_multiple)
#' flag_if_group(tree, "treeID", is_multiple)
#' 
#' by_censusid <- group_by(tree, CensusID)
#' detect_if_group(by_censusid, "treeID", is_multiple)
#' flag_if_group(by_censusid, "treeID", is_multiple)
flag_if_group <- function(.data, 
                          name, 
                          predicate, 
                          condition = warn, 
                          msg = NULL) {
  stopifnot(length(condition) == 1)

  detected <- detect_if_group(.data, name, predicate)
  if (detected) condition(msg %||% glue("{name}: Flagged values were detected."))
  
  invisible(.data)
}

#' @rdname flag_if_group
#' @export
detect_if_group <- function(.data, name, predicate) {
  if (!dplyr::is_grouped_df(.data)) {
    return(detect_if(.data, name, predicate))
  }
  
  g <- dplyr::group_vars(.data)
  lst <- split(.data, .data[g])
  out <- purrr::map(lst, ~detect_if(.x, name, predicate))
  
  any(unlist(out))
}
