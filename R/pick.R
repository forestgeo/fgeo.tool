#' Pick rows from ForestGEO censuses in a list of nested dataframe.
#' 
#' This function allows you to pick rows from a key census (dataframe) in a list
#' or list-column and pick the exact same rows in all other non-key censuses.
#'
#' @param .data A ForestGEO dataset of class 'censuses_lst' (a list) or 
#'   'censuses_df' (a nested dataframe).
#' @param key Key dataframe to pick rows from and and recycle in all other
#'   censuses
#' @param ... Other arguments passed to methods.
#' 
#' @family functions to pick or drop rows of a ForestGEO dataframe
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' censuses_lst <- as_censuses(list(
#'   c1 = tibble(dbh = 1:2),
#'   c2 = tibble(dbh = 8:9)
#' ))
#' censuses_lst
#' class(censuses_lst)
#' 
#' pick(censuses_lst, dbh == 1)
#' pick(censuses_lst, dbh >= 2)
#' pick(censuses_lst, dbh <= 8 , key = "c2")
#' 
#' path <- tool_example("rdata")
#' dir(path)
#' censuses <- read_censuses(path)
#' censuses %>% 
#'   pick(dbh > 600) %>% 
#'   pull(data)
pick <- function(.data, ..., key = NULL) {
  .rowid <- pick_key_rows(.data = .data, ..., key = key)
  out <- pick_all_rows(.data, .rowid)
  as_censuses(out)
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

pull_key_df.censuses_lst <- function(.data, key) {
  
  if (!is.null(key)) {
    stopifnot(length(key) == 1)
    
    valid_key <- key %in% names(.data)
    if (valid_key) 
      return(.data[[key]])
      
    abort_invalid_key(key)
  }
  
  .data[[1]]
}

pull_key_df.censuses_df <- function(.data, key) {
  
  key <- key %||% .data[[1]][[1]]
  stopifnot(length(key) == 1)
  
  key_group <- .data[[1]] %in% key
  if (!any(key_group)) abort_invalid_key(key)
  
  .data[key_group, ]$data[[1]]
}

abort_invalid_key <- function(key) {
  abort(glue("'{key}' not found."))
}

pick_all_rows <- function(.data, ...) {
  UseMethod("pick_all_rows")
}

pick_all_rows.censuses_lst <- function(.data, .rowid) {
  purrr::map(.data, ~.x[.rowid, ])
} 

pick_all_rows.censuses_df <- function(.data, .rowid) {
  dplyr::mutate(.data, data = purrr::map(.data$data, ~.x[.rowid, ]))
} 
