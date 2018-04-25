#' Round to multiple of any number. Copied from `plyr:::round_any.numeric()`.
#' 
#' @param x Numeric vector to round.
#' @param accuracy Number to round to.
#' @param f Rounding function: floor, ceiling or round.
#' 
#' @seealso `plyr::round_any()` and \url{http://bit.ly/2JrBQK3}.
#' 
#' @noRd
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}
