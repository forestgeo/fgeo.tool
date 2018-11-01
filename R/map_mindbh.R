#' lst <- list(
#'   c1 = tibble::tibble(dbh = 1:2),
#'   c1 = tibble::tibble(dbh = 8:9)
#' )
#' 
#' .x <- lst
#' map_mindbh(lst, 2)
#'
#' @param .x 
#' @param mindbh 
#' @param key Name or position of the list element to compute on. The indices
#'   resulting from the computation will be applied to pick rows from all other
#'   list elements. Defaults to the first list element.
map_mindbh <- function(.x, mindbh = 0, key = 1) {
  idx <- .x[[key]]$dbh >= mindbh
  purrr::map(.x, ~.x[idx, ])
}




#' @export
pick_in_sync <- function(.data, ...) {
  UseMethod("pick_in_sync")
}

#' @export
pick_in_sync.default <- function(.data, ...) {
  .class <- glue_collapse(class(.data), sep = ", ", last = " or ")
  abort(glue("Can't deal with data of class {.class}."))
}

# TODO: pick.censuses
# TODO: rename pick.censuses
pick_in_sync.list <- function(.data, ..., key = 1) {
  .dots <- enquos(...)
  
  .key <- tibble::rowid_to_column(.data[[key]])
  .rowid <- dplyr::filter(.key, !!! .dots)$rowid
  
  purrr::map(.data, ~.x[.rowid, ])
}

#' @export
pick_in_sync.tbl <- function(.data, ..., key = 1) {
  .dots <- enquos(...)
  
  key_group <- .data[[1]] %in% key
  key_df <- .data[key_group, ]$data[[1]]
  key_df <- tibble::rowid_to_column(key_df)
  
  .rowid <- dplyr::filter(key_df, !!! .dots)$rowid
  dplyr::mutate(.data, data = purrr::map(data, ~.x[.rowid, ]))
}
