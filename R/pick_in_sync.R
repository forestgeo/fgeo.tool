#' Pick a list of datframes or a nested dataframe in sync.
#' 
#' This function allows you to pick rows from a key dataframe in a list or 
#' list-column and pick the exact same rows in all other non-key dataframes
#' of the list (or list-column).
#'
#' @param .data A list or nested dataframe.
#' @param key Key dataframe to pick rows from and use the same row indices to
#'   pick rows from all other dataframes in the list (or list-column).
#' @param ... Other arguments passed to methods.
#'
#' @export
#'
#' @examples
#' lst <- list(
#'   c1 = tibble::tibble(dbh = 1:2),
#'   c2 = tibble::tibble(dbh = 8:9)
#' )
#' 
#' pick(lst, dbh == 1)
#' pick(lst, dbh >= 2)
#' pick(lst, dbh <= 8 , key = 2)
#' 
#' dfm <- tibble::tribble(
#'   ~dbh, ~census,
#'     1L,       1,
#'     2L,       1,
#'     8L,       2,
#'     9L,       2
#' )
#' 
#' nested_df <- dfm %>% dplyr::group_by(census) %>% nest()
#' 
#' out <- pick(nested_df, dbh == 1)
#' out
#' # Same but ackward
#' out$data
#' 
#' out <- pick(nested_df, dbh >= 1)
#' out
#' # Same but ackward
#' out$data
#' 
#' 
#' censuses <- rdata_df(tool_example("rdata"), .id = "census") %>% 
#'   group_by(census) %>% 
#'   nest()
#' censuses
#' 
#' pick(censuses, dbh > 30, key = 1)
pick <- function(.data, ...) {
  UseMethod("pick")
}

#' @export
pick.default <- function(.data, ...) {
  abort_bad_class(.data)
}

# TODO: pick.censuses
# TODO: rename pick.censuses
#' @export
pick.censuses_lst <- function(.data, ..., key = NULL) {
  .rowid <- pick_key_rows(.data = .data, ..., key = key)
  out <- purrr::map(.data, ~.x[.rowid, ])
  as_censuses(out)
}

#' @export
pick.censuses_tbl <- function(.data, ..., key = NULL) {
  .rowid <- pick_key_rows(.data = .data, ..., key = key)
  out <- dplyr::mutate(.data, data = purrr::map(data, ~.x[.rowid, ]))
  as_censuses(out)
}

pick_key_rows <- function(.data, ..., key) {
  key_df <- tibble::rowid_to_column(pull_key_df(.data, key))
  dplyr::filter(key_df, !!! enquos(...))$rowid
}

pull_key_df <- function(.data, ...) UseMethod("pull_key_df")

pull_key_df.censuses_lst <- function(.data, key) {
  if (!is.null(key)) {
    return(.data[[key]])
  } 
  
  .data[[1]]
}

pull_key_df.censuses_tbl <- function(.data, key) {
  key <- key %||% .data[[1]][[1]]
  key_group <- .data[[1]] %in% key
  
  .data[key_group, ]$data[[1]]
}
