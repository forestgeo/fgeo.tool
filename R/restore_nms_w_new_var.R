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
#' [names_restore_new_var()] is similar to [names_restore()] but specifically
#' targets dataframes that have been mutated with [dplyr::mutate()].
#' [dplyr::mutate()] drops attributes
#' (https://github.com/tidyverse/dplyr/issues/1984), which makes it
#' [names_restore()] useless. attributes.
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
#' names_restore_new_var(mutated, "newvar", old)
#'
#' # Data contains the variable that will be added
#' dfm <- data.frame(X = 1, Y = "a", newvar = "2")
#' (old <- names(dfm))
#' # Lower names
#' (dfm <- rlang::set_names(dfm, tolower))
#' # Add a variable
#' mutated <- dplyr::mutate(dfm, newvar = x + 1)
#' # Restore
#' names_restore_new_var(mutated, "newvar", old)
names_restore_new_var <- function(x, new_var, old_nms) {
  if (!any(length(x) == length(old_nms), length(x) == length(old_nms) + 1)) {
    stop(
      "The length of `x` must equal the number of names in old_nms, or that + 1"
    )
  }

  if (any(grepl(new_var, old_nms))) {
    rlang::set_names(x, old_nms)
  } else {
    rlang::set_names(x, c(old_nms, new_var))
  }
}
