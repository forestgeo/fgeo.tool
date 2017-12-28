#' @export
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



#' Define the status of a tree based on the status of its stems.
#'
#' @param x A ForestGEO table, either a ViewFullTable or a census table.
#' @param cns_id Bare and lowercase name of the variable giving the census
#'   number -- most likely, either censusid or plotcensusnumber.
#'
#' @return The input data set with lowercase names, and with the additional
#'   variable `status_tree`.
#' @export
#'
#' @examples
#' cns <- tibble::tribble(
#'   ~CensusID, ~Tag, ~Status,
#'           1,    1, "alive",
#'           1,    1,  "dead",
#'           1,    2,  "dead",
#'           1,    2,  "dead",
#'           2,    1, "alive",
#'           2,    1, "alive",
#'           2,    2, "alive",
#'           2,    2,  "dead"
#' )
#' status_tree(cns)
#' # A ViewFullTable
#' vft <- dplyr::rename(cns, PlotCensusNumber = CensusID)
#' status_tree(vft, plotcensusnumber)
status_tree <- function(x, cns_id = censusid) {
  old <- names(x)
  x <- rlang::set_names(x, tolower)
  check_status_tree(x)

  cns_id <- rlang::enquo(cns_id)
  grouped <- dplyr::group_by(x, !!cns_id, .data$tag)
  mutated <- dplyr::ungroup(
    dplyr::mutate(
      grouped,
      status_tree = ifelse(all(.data$status == "dead"), "dead", "alive")
    )
  )
  restore_names(mutated, "status_tree", old)
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
