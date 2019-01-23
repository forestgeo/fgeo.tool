#' Guess plot dimensions.
#'
#' @template x_fgeo
#' @param accuracy A number giving the accuracy with which to round `gx` and
#'   `gy`.
#'
#' @return A numeric vector of length 2.
#'
#' @examples
#' x <- data.frame(
#'   gx = c(0, 300, 979),
#'   gy = c(0, 300, 481)
#' )
#' guess_plotdim(x)
#'
#' @family functions for fgeo census and vft
#' @family functions for fgeo census
#' @family functions for fgeo vft
#' @family general functions to find or approximate
#' @family functions for internal use in other fgeo packages
#' @keywords internal
#' @export
guess_plotdim <- function(x, accuracy = 20) {
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(accuracy))

  names(x) <- tolower(names(x))
  .match <- c("x", "gx", "y", "gy", "x", "px")
  matched <- nms_pull_matches(x, .match)

  n_nms <- length(matched)
  if (n_nms != 2) {
    stop("Not enough columns to find x/y positions.\n", matched, call. = FALSE)
  }

  guess <- vapply(
    x[ , c("gx", "gy")], guess_max, double(1), accuracy = accuracy
  )

  message("Guessing: plotdim = c(", commas(guess), ")")
  unname(guess)
}

#' Pull names that match a character vector.
#'
#' The `nms_` prefix matches functions in other fgeo packages.
#'
#' @param x A named object.
#' @param .match A character vector giving names to match.
#'
#' @return A character vector.
#'
#' @examples
#' nms_pull_matches(luquillo_stem_random_tiny, c("x", "PX", "gx"))
#' nms_pull_matches(luquillo_vft_4quad, c("x", "PX", "gx"))
#' nms_pull_matches(luquillo_vft_4quad, c("PY", "PX", "gx", "gy"))
#'
#' @family general functions to deal with names
#' @family functions for developers
#' @keywords internal
#' @noRd
nms_pull_matches <- function(x, .match) {
  stopifnot(rlang::is_named(x))
  names(x)[grepl(pipes(anchor(.match)), names(x))]
}



#' Guess maximum value of a vector with flexible accuracy.
#'
#' @param x Numeric vector.
#' @param accuracy A single number.
#'
#' @return A number.
#'
#' @examples
#' guess_max(1:19, 20)
#' 
#' @family general functions to find or approximate
#' @keywords internal
#' @noRd
guess_max <- function(x, accuracy) {
  max_x <- max(x, na.rm = TRUE)
  round_any(max_x, f = ceiling, accuracy = accuracy)
}

