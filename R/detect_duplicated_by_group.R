#' Factories to detect and flag conditions on a variable by groups.
#' 
#' These funcions extend [detect_f()] to work by groups defined with
#' [dplyr::group_by()].
#'
#' @section Arguments to the resulting function:
#' * `.data`: A dataframe.
#' * `msg`: String; an optional custom
#'   message.
#'
#' @family functions to check inputs.
#' @family functions for developers.
#' @family predicates.
#' @family function factories.
#'
#' @return A function.
#'
#' @export
#' @examples
#' tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 2))
#' detect_multiple_by_group_f("treeid")(tree)
#' 
#' # Case-insensitive
#' detect_multiple_by_group_f("treeid")(tree)
#' flag_multiple_by_group_f("treeID")(tree)
#' 
#' # Can also output messages and errors
#' flag_multiple_by_group_f("treeID", rlang::inform)(tree)
#' 
#' # Takes custom messages
#' flag_multiple_by_group_f("treeID")(tree, "Custom message")
#' 
#' # Handles grouped data
#' by_censusid <- group_by(tree, CensusID)
#' # Silent
#' flag_multiple_by_group_f("treeID")(by_censusid)
#' 
#' tree <- tibble::tibble(CensusID = c(1, 2), treeID = c(1, 1))
#' detect_duplicated_by_group_f("treeid")(tree)
#' 
#' by_censusid <- group_by(tree, CensusID)
#' # Silent
#' flag_duplicated_by_group_f("treeID")(by_censusid)
detect_duplicated_by_group_f <- function(name) {
  force(name)
  function(.data) {
    any(t(by_group(.data, function(x) detect_if(x, name, is_duplicated))))
  }
}

#' @rdname detect_duplicated_by_group_f
#' @export
detect_multiple_by_group_f <- function(name) {
  force(name)
  function(.data) {
    any(t(by_group(.data, function(x) detect_if(x, name, is_multiple))))
  }
}

flag_predicate_by_group_f <- function(name, cond, predicate, prefix) {
  force(name)
  function(.data, msg = NULL) {
    stopifnot(length(cond) == 1)

    detected <- any(t(by_group(.data, predicate(name))))
    if (detected) cond(msg %||% glue("{name}: {prefix} values were detected."))

    invisible(.data)
  }
}

#' @rdname detect_duplicated_by_group_f
#' @export
flag_duplicated_by_group_f <- function(name, cond = warn) {
  flag_predicate_by_group_f(name, cond, detect_duplicated_f, "Duplicated")
}

#' @rdname detect_duplicated_by_group_f
#' @export
flag_multiple_by_group_f <- function(name, cond = warn) {
  flag_predicate_by_group_f(name, cond, detect_multiple_f, "Multiple")
}

detect_multiple_f <- function(name) {
  function(.data) detect_if(.data, name, is_multiple)
}

detect_duplicated_f <- function(name) {
  function(.data) detect_if(.data, name, is_duplicated)
}









flag_if_by_group <- function(.data, 
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
