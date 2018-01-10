#' Lower and restore names.
#'
#' These functions are useful together, to lowercase names, then to do something
#' (as long as attributes are preserved -- see section warning), and then
#' restore the original names.
#'
#' @section Warning:
#' [names_restore()] is similar to [names_restore_new_var()] but
#' [names_restore()] is necesary if the data is mutated with [dplyr::mutate()]:
#' [dplyr::mutate()] drops attributes
#' (https://github.com/tidyverse/dplyr/issues/1984), which makes it
#' [names_restore()] useless. attributes.
#'
#' @param x A named object.
#'
#' @family functions for developers.
#'
#' @return
#' * `names_lowercase()` Returns the object `x` with lowercase names
#' * `names_restore()` Returns the object `x` with original (restored) names.
#' @export
#'
#' @examples
#' cns <- tibble::tibble(CensusID = 1, status = "A")
#' original <- cns
#' original
#'
#' lowered <- names_lowercase(cns)
#' lowered
#' attr(lowered, "names_old")
#'
#' back_to_original <- names_restore(lowered)
#' back_to_original
names_lowercase <- function(x) {
  is_not_named <- is.null(attr(x, "names"))
  if (is_not_named) {stop("`x` must be named")}

  attr(x, "names_old") <- names(x)
  x <- rlang::set_names(x, tolower)
  x
}

#' @name names_lowercase
#' @export
names_restore <- function(x) {
  x_has_attr_names_old <- !is.null(attr(x, "names_old"))
  stopifnot(x_has_attr_names_old)

  names(x) <- attr(x, "names_old")
  x
}
