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
#' @family internal functions for developers.
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
#' @family internal functions for developers.
nms_minus_lower_nms <- function(table1, table2) {
  stopifnot(is.data.frame(table1), is.data.frame(table2))
  
  nms_list <- purrr::map(list(table1, table2), names)
  diff_nms <- purrr::reduce(nms_list, setdiff)
  nms_list_lowered <- purrr::map(nms_list, tolower)
  diff_nms_lower <- purrr::reduce(nms_list_lowered, setdiff)
  length(diff_nms) - length(diff_nms_lower)
}
