#' Check if an object contains specific names.
#'
#' @param x A named object.
#' @param nms String; names expected to be found in `x`.
#'
#' @family functions to check inputs.
#' @family functions for developers
#' @family general functions to assert
#'
#'
#' @return Invisible `x`, or an error with informative message.
#' 
#' @family exported functions for internal use
#' @export
#' @keywords internal
#'
#' @examples
#' v <- c(x = 1)
#' check_crucial_names(v, "x")
#'
#' dfm <- data.frame(x = 1)
#' check_crucial_names(dfm, "x")
check_crucial_names <- function(x, nms) {
  stopifnot(is_named(x))
  stopifnot(is.character(nms))

  are_names_expected <- all(nms %in% names(x))
  if (are_names_expected) {
    return(invisible(x))
  }

  stop(
    "Ensure your data set has these variables:\n", glue_comma(nms),
      call. = FALSE
  )
}

#' Is named object? (Base-only version of rlang::is_named).
#'
#' @param x An object to test.
#'
#' @family general predicates
#'
#' @return The scalar `TRUE` or `FALSE`.
#' 
#' @family functions for internal use in other fgeo packages
#' @keywords internal
#' @export
#'
#' @examples
#' is_named(c(x = 1))
#' is_named(c(1))
#' is_named(x = 1)
#'
#' is_named(data.frame(x = 1))
#'
#' dfm <- data.frame(1)
#' is_named(dfm)
#' dfm
is_named <- function (x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(FALSE)
  }
  if (any(nms == "" | is.na(nms))) {
    return(FALSE)
  }
  TRUE
}
