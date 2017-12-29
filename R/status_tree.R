#' @export
add_status_tree <- function(x) {
  old <- names(x)
  x <- rlang::set_names(x, tolower)
  check_add_status_tree(x)
  grouped <- dplyr::group_by(x, .data$censusid, .data$tag)
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
  crucial_vars <- c("tag", "status", "censusid")
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
#' @template x_fgeo
#'
#' @return The input data set with the additional variable `status_tree`.
#' @export
#'
#' @examples
#' x <- tibble::tribble(
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
#' status_tree(x)
status_tree <- function(x) {
  old <- names(x)
  x <- rlang::set_names(x, tolower)
  check_status_tree(x)

  grp <- dplyr::group_by(x, .data$censusid, .data$tag)
  mut <- dplyr::mutate(grp, status_tree = ifelse(all(.data$status == "dead"), "dead", "alive"))
  restore_names(dplyr::ungroup(mut), "status_tree", old)
}

check_status_tree <- function(x) {
  stopifnot(is.data.frame(x))
  check_crucial_names(x, c("tag", "status", "censusid"))
  is_vft <- "plotid" %in% names(x)
  if (is_vft) {
    check_unique(
      x, "plotid",
      "stop", msg = "  * Filter your data to keep a single plot and try again"
    )
  }
}
