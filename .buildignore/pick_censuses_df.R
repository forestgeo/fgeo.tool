pull_key_df.censuses_df <- function(.data, key) {
  key <- key %||% .data[[1]][[1]]
  stopifnot(length(key) == 1)
  
  key_group <- .data[[1]] %in% key
  if (!any(key_group)) abort_invalid_key(key)
  
  .data[key_group, ]$data[[1]]
}

pick_all_rows.censuses_df <- function(.data, .rowid) {
  dplyr::mutate(.data, data = purrr::map(.data$data, ~.x[.rowid, ]))
} 
