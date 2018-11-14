#' Make any function work with grouped data.
#'
#' @param .x A dataframe.
#' @param .f A function.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return The value of `.f` applied to groups.
#' @keywords internal
#' 
#' @section Acknowledgments:
#' [Tristan Mahr](https://www.tjmahr.com/) and [Edward
#' Visel](https://alistaire.rbind.io/) helped improve this function
#' ([post](http://bit.ly/2uDrf97)).
#' 
#' @export
#'
#' @examples
#' dfm <- tibble::tribble(
#'   ~x, ~y,  ~z,
#'   11, 21,  31,
#'   12, 22,  32,
#'   13, 23,  33
#' )
#' 
#' # Say you want to make this function work with grouped data
#' first_row <- function(.x, to_chr = FALSE) {
#'   first <- .x[1, ]
#'   if (to_chr) {
#'     first[] <- lapply(first, as.character)
#'   }
#'   
#'   tibble::as.tibble(first)
#' }
#' 
#' library(dplyr)
#' 
#' # Ungrouped
#' dfm %>% first_row()
#' # Same -- it does nothing with ungrouped data
#' dfm %>% by_group(first_row)
#' 
#' # Grouped
#' dfm %>% group_by(x) %>% by_group(first_row)
#' 
#' dfm %>% group_by(x) %>% by_group(first_row, to_chr = TRUE)
by_group <- function(.x, .f, ...) {
  stopifnot(is.data.frame(.x), is.function(.f))

  if (!dplyr::is_grouped_df(.x)) {
    return(.f(.x, ...))
  }

  g <- dplyr::group_vars(.x)

  split <- split(.x, dplyr::group_indices(.x))
  .split <- purrr::map(split, ungroup)
  out <- purrr::map_df(set_names(.split, names(.x)), .f, ...)

  dplyr::grouped_df(out, vars = g)
}


