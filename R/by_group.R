#' Make any function work with grouped data.
#'
#' @param .x A dataframe.
#' @param .f A function.
#' @param ... Addditional arguments passed to `.f`.
#'
#' @return The value of `.f` applied to groups with this caveats:
#' * Following the approach split/apply/combine, `.f` does not compute on the 
#' grouping variables.
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
#' # Say you want to make this funciton work with grouped data
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
#' # Computes on any non-groping variable and returns the original groups
#' dfm %>% group_by(x) %>% by_group(first_row, to_chr = TRUE)
by_group <- function(.x, .f, ...) {
  stopifnot(is.data.frame(.x), is.function(.f))
  
  nested <- tidyr::nest(.x)
  out <- dplyr::mutate(nested, data = purrr::map(.data$data, .f, ...))
  out <- tidyr::unnest(out)

  if (dplyr::is.grouped_df(.x)) {
    out <- dplyr::grouped_df(out, dplyr::group_vars(.x))
  }
  
  out
}
