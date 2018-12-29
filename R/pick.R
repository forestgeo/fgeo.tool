#' Pick rows from a list of dataframes (e.g. a list of ForestGEO censuses).
#' 
#' This function allows you to pick rows from a `key` dataframe in a list
#' and pick the same row indices in all other non-key censuses. The conditions
#' to pick rows are checked against the `key` dataframe only.
#'
#' @param .data A list of dataframes.
#' @param key Dataframe used to check the conditions passed via `...`.
#' @param ... Other arguments passed to methods.
#'
#' @examples
#' censuses <- list(
#'   c1 = tibble(dbh = 1:2),
#'   c2 = tibble(dbh = 8:9)
#' )
#' 
#' censuses
#' 
#' pick(censuses, dbh == 1, key = "c1")
#' 
#' # Same because `key` defaults to the first element of the list
#' pick(censuses, dbh == 1)
#' 
#' # Different `key`
#' pick(censuses, dbh == 1, key = "c2")
#' 
#' # With multiple conditions passed via `...`
#' pick(censuses, dbh >= 1, dbh < 2)
#' 
#' # `pick()` is useful after reading multiple censuses into a list
#' rdata_files <- tool_example("rdata")
#' dir(rdata_files)
#' 
#' censuses <- rdata_list(rdata_files)
#' pick(censuses, dbh >= 100)
#' 
#' # Same in a pipeline
#' rdata_files %>%
#'   rdata_list() %>%
#'   pick(dbh >= 100)
#' 
#' @family general functions to pick or drop rows of a dataframe
#' @export
pick <- function(.data, ..., key = NULL) {
  .rowid <- pick_key_rows(.data = .data, ..., key = key)
  pick_all_rows(.data, .rowid)
}

pick_key_rows <- function(.data, ..., key) {
  key_df <- tibble::rowid_to_column(pull_key_df(.data, key))
  dplyr::filter(key_df, !!! enquos(...))$rowid
}

pull_key_df <- function(.data, ...) {
  UseMethod("pull_key_df")
}

pull_key_df.default <- function(.data, ...) {
  abort_bad_class(.data)
}

pull_key_df.list <- function(.data, key) {
  
  if (!is.null(key)) {
    stopifnot(length(key) == 1)
    
    valid_key <- key %in% names(.data)
    if (valid_key) 
      return(.data[[key]])
      
    abort_invalid_key(key)
  }
  
  .data[[1]]
}

abort_invalid_key <- function(key) {
  abort(glue("'{key}' not found."))
}

pick_all_rows <- function(.data, ...) {
  UseMethod("pick_all_rows")
}

pick_all_rows.list <- function(.data, .rowid) {
  purrr::map(.data, ~.x[.rowid, ])
}
