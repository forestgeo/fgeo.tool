#' Lower and restore names.
#'
#' These functions are useful together, to lowercase names, then to do something
#' (as long as attributes are preserved -- see section warning), and then
#' restore the original names.
#'
#' @section Warning:
#' [nms_restore()] is similar to [nms_restore_newvar()] but
#' [nms_restore()] is necesary if the data is mutated with [dplyr::mutate()]:
#' [dplyr::mutate()] drops attributes
#' (https://github.com/tidyverse/dplyr/issues/1984), which makes it
#' [nms_restore()] useless. attributes.
#'
#' @param x A named object.
#'
#' @family functions for developers.
#'
#' @return
#' * `nms_lowercase()` Returns the object `x` with lowercase names
#' * `nms_restore()` Returns the object `x` with original (restored) names.
#' @export
#'
#' @examples
#' cns <- tibble::tibble(CensusID = 1, status = "A")
#' original <- cns
#' original
#'
#' lowered <- nms_lowercase(cns)
#' lowered
#' attr(lowered, "names_old")
#'
#' back_to_original <- nms_restore(lowered)
#' back_to_original
nms_lowercase <- function(x) {
  is_not_named <- is.null(attr(x, "names"))
  if (is_not_named) {stop("`x` must be named")}

  attr(x, "names_old") <- names(x)
  x <- set_names(x, tolower)
  x
}

#' @name nms_lowercase
#' @export
nms_restore <- function(x) {
  x_has_attr_names_old <- !is.null(attr(x, "names_old"))
  stopifnot(x_has_attr_names_old)

  names(x) <- attr(x, "names_old")
  x
}



#' Restore the names of a dataframe to which a new variable has been added.
#'
#' This function helps to develop functions that work with both ViewFullTables
#' and census tables. ViewFullTables and census tables share multiple names but
#' often the case of those names is different (e.g. `Tag` and `tag`). When
#' developing functions that work with both ViewFullTables and fgeo tables, one
#' solution is to lowercase all names, do whatever the function needs to do, and
#' then restore the old names. This function helps by restoring old names, which
#' is not straight forward when the function adds a new variable and may contain
#' a preexising variable with the same name of the added variable.
#'
#' The length of `x` must equal the number of names in old_nms, or that + 1".
#'
#' [nms_restore_newvar()] is similar to [nms_restore()] but specifically
#' targets dataframes that have been mutated with [dplyr::mutate()].
#' [dplyr::mutate()] drops attributes
#' (https://github.com/tidyverse/dplyr/issues/1984), which makes it
#' [nms_restore()] useless. attributes.
#'
#' @param x A dataframe.
#' @param new_var The name of a single new variable added to `x`.
#' @param old_nms A vector containing the old names of `x`.
#'
#' @family functions for developers.
#'
#' @return Returns the input with the names changed accordingly.
#' @export
#'
#' @examples
#' # Data does not contain the variable that will be added
#' dfm <- data.frame(X = 1, Y = "a")
#' (old <- names(dfm))
#' # Lower names
#' (dfm <- rlang::set_names(dfm, tolower))
#' # Add a variable
#' mutated <- dplyr::mutate(dfm, newvar = x + 1)
#' # Restore
#' nms_restore_newvar(mutated, "newvar", old)
#'
#' # Data contains the variable that will be added
#' dfm <- data.frame(X = 1, Y = "a", newvar = "2")
#' (old <- names(dfm))
#' # Lower names
#' (dfm <- rlang::set_names(dfm, tolower))
#' # Add a variable
#' mutated <- dplyr::mutate(dfm, newvar = x + 1)
#' # Restore
#' nms_restore_newvar(mutated, "newvar", old)
nms_restore_newvar <- function(x, new_var, old_nms) {
  if (!any(length(x) == length(old_nms), length(x) == length(old_nms) + 1)) {
    stop(
      "The length of `x` must equal the number of names in old_nms, or that + 1"
    )
  }
  
  if (any(grepl(new_var, old_nms))) {
    set_names(x, old_nms)
  } else {
    set_names(x, c(old_nms, new_var))
  }
}
