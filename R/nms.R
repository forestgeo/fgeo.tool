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
#' @family functions to manipulate names.
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
#' @family functions to manipulate names.
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


# Handy -------------------------------------------------------------------

# Internal functions not used. Consider removing them.

#' Functions to detect and extract names.
#' 
#' These functions are handy to work with fgeo's data structures because the
#' same variablel may be named differently in different data sets. For example,
#' the variable status is called `Status` or `status` in viewfull or census
#' (tree and stem) tables.
#' 
#' nms_has_any(): Checks if an object has any of the provided names.
#' * Returns a logical value.
#' nms_detect(): Checks if an object has the provided names. 
#' * Returns a logical vector.
#' nms_extract_all(): Finds the names that match the provided names. 
#' * Returns a character vector.
#' nms_extract1(): Finds the first name that matchs the provided names.
#' * Returns a character string.
#'
#' @param x A named object.
#' @param ... Strings; each of the names that need to be checked.
#' 
#' @examples
#' \dontrun{
#' v <- c(a = 1, b = 1)
#' nms_has_any(v, "a", "B")
#' nms_has_any(v, "A", "B")
#' nms_has_any(v, "A", "b")
#' 
#' nms_detect(v, "a", "B", "b")
#' 
#' nms_extract_all(v, "a", "B")
#' nms_extract_all(v, "a", "a", "b")
#' 
#' nms_extract1(v, "a", "a", "b")
#' }
#' @family functions for developers.
#' @family functions to manipulate names.
#' @name nms
NULL

#' @rdname nms
nms_has_any <- function(x, ...) {
  any(nms_detect(x, ...))
}

#' @rdname nms
nms_detect <- function(x, ...) {
  purrr::map_lgl(list(...), ~rlang::has_name(x, .))
}

#' @rdname nms
nms_extract_all <- function(x, ...) {
  is_detected <- nms_detect(x, ...)
  nms <- unlist(list(...))
  unique(nms[is_detected])
}

#' @rdname nms
nms_extract1 <- function(x, ...) {
  extracted <- nms_extract_all(x, ...)
  if (length(extracted) == 0) {
    extracted
  } else {
    extracted[[1]]
  }
}



#' Find a name exactly matching a string but regardless of case.
#'
#' @param x A named object.
#' @param nm A string to match names exactly but regardless of case.
#'
#' @return A string of the name that was found in `names(x)`.
#' @export
#'
#' @examples
#' v <- c(a = 1, B = 1)
#' nms_extract_anycase(v, "b")
#' 
#' dfm <- data.frame(a = 1, B = 1)
#' nms_extract_anycase(dfm, "b")
#' @family functions for developers.
#' @family functions to manipulate names.
nms_extract_anycase <- function(x, nm) {
  has_nms <- !is.null(attr(x, "names"))
  stopifnot(has_nms, is.character(nm))
  names(x)[which(nm  ==  tolower(names(x)))]
}

#' Comparing two dataframes, How many names differ only in case?
#'
#' @param table1 A dataframe
#' @param table2 A dataframe
#'
#' @return An number indicating how many names are different only in their case.
#'
#' @examples
#' \dontrun{
#' vft <- yosemite::ViewFullTable_yosemite
#' stem <- yosemite::yosemite_s1_lao
#' tree <- yosemite::yosemite_f1_lao
#' nms_minus_lower_nms(stem, vft)
#' nms_minus_lower_nms(vft, stem)
#' nms_minus_lower_nms(stem, tree)
#' }
#' @family functions for developers.
#' @family functions to manipulate names.
nms_minus_lower_nms <- function(table1, table2) {
  stopifnot(is.data.frame(table1), is.data.frame(table2))
  
  nms_list <- purrr::map(list(table1, table2), names)
  diff_nms <- purrr::reduce(nms_list, setdiff)
  nms_list_lowered <- purrr::map(nms_list, tolower)
  diff_nms_lower <- purrr::reduce(nms_list_lowered, setdiff)
  length(diff_nms) - length(diff_nms_lower)
}
