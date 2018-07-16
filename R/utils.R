commas <- function(...) {
  paste0(..., collapse = ", ")
}

has_class_df <- function(x) {
  any(grepl("data.frame", class(x)))
}

each_list_item_is_df <- function(x) {
  if (!is.list(x) || is.data.frame(x)) {
    abort("`x` must be a list of datafraems (and not itself a dataframe).")
  }
  all(vapply(x, has_class_df, logical(1)))
}

multiple_censusid <- fgeo.base::multiple_var("censusid")

multiple_plotname <- fgeo.base::multiple_var("plotname")

groups_lower <- function(.data) {
  dplyr::grouped_df(.data, tolower(dplyr::group_vars(.data)))
}

groups_restore <- function(to_set, ref) {
  dplyr::grouped_df(to_set, dplyr::group_vars(ref))
}


