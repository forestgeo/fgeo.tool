#' Predicates to detect and flag duplicated and multiple values of a variable.
#'
#' `is_multiple()` and `is_duplicated()` return `TRUE` if they detect,
#' respectively, multiple different values of a variable (e.g. c(1, 2)`), or
#' duplicated values of a variable (e.g. c(1, 1)`).
#'
#' @param .data A vector.
#'
#' @family functions to check inputs.
#' @family functions for developers
#' @family general predicates
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' is_multiple(c(1, 2))
#' is_multiple(c(1, 1))
#' is_multiple(c(1, NA))
#'
#' is_duplicated(c(1, 2))
#' is_duplicated(c(1, 1))
#' is_duplicated(c(1, NA))
is_multiple <- function(.data) {
  length(unique(stats::na.omit(.data))) > 1
}

#' @rdname is_multiple
#' @keywords internal
#' @noRd
is_duplicated <- function(.data) {
  any(duplicated(.data))
}
