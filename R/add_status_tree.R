#' Define the status of a tree based on the status of its stems.
#'
#' @param x A ViewFullTable or a fgeo table.
#'
#' @return The input data set with lowercase names, and with the additional
#'   variable status_tree.
#' @export
#'
#' @examples
#' df <- tibble::tribble(
#'   ~PlotCensusNumber, ~Tag, ~Status,
#'                   1,    1, "alive",
#'                   1,    1,  "dead",
#'                   1,    2,  "dead",
#'                   1,    2,  "dead",
#'
#'                   2,    1, "alive",
#'                   2,    1, "alive",
#'                   2,    2, "alive",
#'                   2,    2,  "dead"
#' )
#' add_status_tree(df)
add_status_tree <- function(x) {
  old <- names(x)
  x <- rlang::set_names(x, tolower)
  check_add_status_tree(x)
  grouped <- dplyr::group_by(x, .data$plotcensusnumber, .data$tag)
  mutated <- dplyr::mutate(
    grouped,
    status_tree = ifelse(all(.data$status == "dead"), "dead", "alive")
  )
  # Restoring names
  if (any(grepl("status_tree", old))) {
    rlang::set_names(dplyr::ungroup(mutated), old)
  } else {
    rlang::set_names(dplyr::ungroup(mutated), c(old, "status_tree"))
  }
}

check_add_status_tree <- function(x) {
  is_vft <- "plotid"  %in% names(x)
  if (is_vft) check_unique_plotid(x)
  crucial_vars <- c("tag", "status", "plotcensusnumber")
  check_crucial_names(x, crucial_vars)
  invisible(x)
}

check_unique_plotid <- function(x) {
  msg <- "  * Filter your data to keep a single plot; then try again"
  check_unique(x, "plotid", "stop", msg)
  invisible(x)
}

