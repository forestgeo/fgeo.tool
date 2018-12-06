#' Round to multiple of any number. Copied from `plyr:::round_any.numeric()`.
#'
#' @param x Numeric vector to round.
#' @param accuracy Number to round to.
#' @param f Rounding function: floor, ceiling or round.
#'
#' @seealso `plyr::round_any()` and \url{http://bit.ly/2JrBQK3}.
#' @family functions for developers
#' @family functions dealing with names.
#' @family functions for developers with no dependencies.
#' @family general functions to find or approximate
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' # From pryr::round_any()
#' round_any(135, 10)
#' round_any(135, 100)
#' round_any(135, 25)
#' round_any(135, 10, floor)
#' round_any(135, 100, floor)
#' round_any(135, 25, floor)
#' round_any(135, 10, ceiling)
#' round_any(135, 100, ceiling)
#' round_any(135, 25, ceiling)
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}
