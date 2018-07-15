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


# Predicates --------------------------------------------------------------

# For examples see tests/testthat/test-utils.R
multiple_var <- function(var) {
  force(var)
  var <- tolower(var)
  function(.data) {
    .data <- stats::setNames(.data, tolower(names(.data)))
    .var <- .data[[var]]
    var %in% names(.data) && length(unique(stats::na.omit(.var))) > 1
  }
}

multiple_censusid <- multiple_var("censusid")
multiple_plotname <- multiple_var("plotname")
