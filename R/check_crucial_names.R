#' Check if an object contains specific names.
#'
#' @param x A named object.
#' @param nms String; names expected to be found in `x`.
#'
#' @return Invisible `x`, or an error with informative message.
#'
#' @examples
#' v <- c(x = 1)
#' check_crucial_names(v, "x")
#' 
#' dfm <- data.frame(x = 1)
#' check_crucial_names(dfm, "x")
#' @family functions to check inputs
#' @family general functions to assert
#' @family functions for developers
#' @family exported functions for internal use
#' @keywords internal
#' @export
check_crucial_names <- function(x, nms) {
  stopifnot(rlang::is_named(x))
  stopifnot(is.character(nms))

  are_names_expected <- all(nms %in% names(x))
  if (are_names_expected) {
    return(invisible(x))
  }

  stop(
    "Ensure your data set has these variables:\n", commas(nms),
    call. = FALSE
  )
}
