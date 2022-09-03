#' Predicates to detect and flag duplicated and multiple values of a variable.
#'
#' `is_multiple()` and `is_duplicated()` return `TRUE` if they detect,
#' respectively, multiple different values of a variable (e.g. c(1, 2)`), or
#' duplicated values of a variable (e.g. c(1, 1)`).
#'
#' @param .data A vector.
#'
#' @return Logical.
#'
#' @examples
#' is_multiple(c(1, 2))
#' is_multiple(c(1, 1))
#' is_multiple(c(1, NA))
#'
#' is_duplicated(c(1, 2))
#' is_duplicated(c(1, 1))
#' is_duplicated(c(1, NA))
#' @family functions for internal use in other fgeo packages
#' @family functions to check inputs
#' @family functions for developers
#' @family general predicates
#' @keywords internal
#' @export
is_multiple <- function(.data) {
  length(unique(stats::na.omit(.data))) > 1
}

#' @rdname is_multiple
#' @export
is_duplicated <- function(.data) {
  any(duplicated(.data))
}
