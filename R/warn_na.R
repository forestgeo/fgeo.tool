#' Throw condition if variable has missing values.
#'
#' @param x A vector or dataframe.
#'
#' @family functions to throw conditions
#'
#' @return A warning message and invisible `x`.
#'
#' @examples
#' warn_na(c(x = 1, y = NA))
#' @name condition_na
#' @noRd
warn_na <- function(x) {
  stopifnot(
    is.data.frame(x) || is.vector(x), 
    !is.null(names(x)), rlang::is_named(x)
  )

  out <- vapply(x, function(x) any(is.na(x)), logical(1))
  has_na <- out
  if (any(has_na)) {
    warning("Detected missing values in: ", commas(names(x)[has_na]))
  }
  
  invisible(x)
}
