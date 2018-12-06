anchor <- function(x) paste0("^", x, "$")

commas <- function(...) paste0(..., collapse = ", ")
glue_comma <- commas

#' Round to multiple of any number. Copied from `plyr:::round_any.numeric()`.
#' @noRd
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
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

groups_lower <- function(x) {
  dplyr::grouped_df(x, tolower(dplyr::group_vars(x)))
}

groups_restore <- function(x, ref) {
  dplyr::grouped_df(x, dplyr::group_vars(ref))
}

stopifnot_single_plotname <- function(.x) {
  if (has_name(.x, "plotname") && detect_if(.x, "plotname", is_multiple)) {
    stop("`.x` must have a single plotname.", call. = FALSE)
  }
}

abort_bad_class <- function(x) {
  .class <- glue_collapse(class(x), sep = ", ", last = ", or ")
  abort(glue("Can't deal with data of class: {.class}."))
}
