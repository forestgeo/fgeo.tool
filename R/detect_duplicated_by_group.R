#' Factories to detect and flag conditions on a variable by groups.
#' 
#' These funcions extend [detect_multiple_f()] and friends to work by groups
#' defined with [dplyr::group_by()].
#'
#' @inheritParams fgeo.base::detect_multiple_f
#' 
#' @section Arguments to the resulting function:
#' * .data (Argument to the resulting function) A dataframe.
#' * msg (Argument to the resulting function) String; an optional custom
#'   message.
#'
#' @seealso [detect_multiple_f()], [flag_multiple_f()], [detect_duplicated_f()],
#' [flag_duplicated_f()].
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
  function(.data) any(t(by_group(.data, fgeo.base::detect_duplicated_f(name))))
}

#' @rdname detect_duplicated_by_group_f
#' @export
detect_multiple_by_group_f <- function(name) {
  force(name)
  function(.data) any(t(by_group(.data, fgeo.base::detect_multiple_f(name))))
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
  flag_predicate_by_group_f(
    name, cond, fgeo.base::detect_duplicated_f, "Duplicated"
  )
}

#' @rdname detect_duplicated_by_group_f
#' @export
flag_multiple_by_group_f <- function(name, cond = warn) {
  flag_predicate_by_group_f(
    name, cond, fgeo.base::detect_multiple_f, "Multiple"
  )
}
