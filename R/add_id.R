#' Add column `id` to a dataframe by pasting values from other columns.
#'
#' @param .data A dataframe.
#' @param vars Vector of names of columns to paste for creating the new id
#'   column.
#' @param name String giving the name of the new id column.
#' @param sep String to separate the pasted values of `vars`.
#'
#' @return A dataframe with an additional column.
#'
#' @examples
#' .data <- tibble::tibble(x = 1:2, y = c("a", "b"), z = c(TRUE, FALSE))
#' add_id(.data, c("x", "y", "z"))
#' add_id(.data, c("x", "z"))
#' add_id(.data, c("x", "z"), sep = 1)
#' 
#' @family functions to add columns to dataframes
#' @export
add_id <- function(.data, vars, name = "id", sep = "_") {
  stopifnot(is.data.frame(.data), is.character(vars), is.character(name))
  
  .vars <- lapply(.data[vars], as.character)
  id <- purrr::pmap_chr(.vars, sep_dots, sep = as.character(sep))
  out <- tibble::add_column(.data, id)
  set_names(out, c(names(.data), name))
}

sep_dots <- function(..., sep = "_") {
  paste(unlist(list(...)), collapse = sep)
}
