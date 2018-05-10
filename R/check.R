# Check -------------------------------------------------------------------

#' Report if a vector or a variable of a dataframe is duplicated.
#'
#' @param x A dataframe.
#' @param x_var String; the name of a variable of `x`.
#' @param v A vector.
#' @param cond String; the name of a function that outputs a condition: one of
#'   "warning", "stop", "message".
#' @param msg String; a custom message.
#'
#' @return Invisible `v` or a condition and a message.
#' @family functions to check inputs.
#' @family functions for developers.
#' @export
#'
#' @examples
#' # On a vector
#' unique_v <- rep(1, 3)
#' num <- c(1:3)
#' chr <- c(letters[1:3])
#' check_unique_vector(unique_v, "warning")
#' check_unique_vector(num, "warning")
#' check_unique_vector(chr, "message", "Do something")
#'
#' # On a dataframe
#' .df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)
#'
#' check_unique(.df, "a")
#' check_unique(.df, "a", "message", "do this")
#' # Silent
#' check_unique(.df, "b", "warning", "do this")
check_unique <- function(x, x_var, cond = "warning", msg = NULL) {
  stopifnot(is.data.frame(x))
  if (!x_var  %in% names(x)) stop(x_var, " is an invalid name")

  x_var <- x[[x_var]]
  check_unique_vector(v = x_var, cond = cond, msg = msg)
  invisible(x)
}

#' @rdname check_unique
#' @export
check_unique_vector <- function(v, cond, msg = NULL) {
  stopifnot(length(cond) == 1)
  stopifnot(cond %in% c("warning", "stop", "message"))

  customized <- c("Duplicated values were detected\n", msg)
  if (length(unique(v)) > 1) {
    do.call(cond, list(customized))
  }
  invisible(v)
}
