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


status_tree <- function(x, cns_id = censusid) {
  old <- names(x)
  x <- rlang::set_names(x, tolower)
  check_status_tree(x)

  cns_id <- rlang::enquo(cns_id)
  grouped <- dplyr::group_by(x, !!cns_id, .data$tag)
  with_status_tree <- dplyr::ungroup(
    dplyr::mutate(
      grouped,
      status_tree = ifelse(all(.data$status == "dead"), "dead", "alive")
    )
  )

  rlang::set_names(with_status_tree, c(old, "status_tree"))
}

check_status_tree <- function(x) {
  stopifnot(is.data.frame(x))
  check_crucial_names(x, c("tag", "status"))
  if ("plotid" %in% names(x)) {
    check_unique(
      x, "plotid",
      "stop", msg = "  * Filter your data to keep a single plot and try again"
    )
  }
}

status_tree(bciex::bci12t7mini)
status_tree(bciex::bci12vft_mini)
status_tree(bciex::bci12vft_mini, plotcensusnumber)
status_tree(1, plotcensusnumber)

