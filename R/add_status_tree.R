#' Define the status of a tree based on the status of its stems.
#'
#' @template x_fgeo
#' @param status_a,status_d Sting to match alive and dead stems; it corresponds
#'   to the values of the variable `status` (in census tables) or `Status` (with
#'   capital "S" in ViewFull tables).
#' @family functions to add columns to dataframes.
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
#' add_status_tree(x)
add_status_tree <- function(x, status_a = "A", status_d = "D") {

  old <- names(x)
  x <- set_names(x, tolower)
  check_add_status_tree(x, status_a = status_a, status_d = status_d)

  grp <- group_by(x, .data$censusid, .data$tag)
  mut <- mutate(
    grp, status_tree = ifelse(
      all(.data$status == status_d),
      status_d,
      status_a
    )
  )
  nms_restore_newvar(ungroup(mut), "status_tree", old)
}

check_add_status_tree <- function(x, status_d, status_a) {
  stopifnot(is.data.frame(x))
  check_crucial_names(x, c("tag", "status", "censusid"))
  check_valid_status(x, .status = c(status_d, status_a), "status")
  is_vft <- "plotid" %in% names(x)
  if (is_vft) {
    check_unique(
      x, "plotid",
      "stop", msg = "  * Filter your data to keep a single plot and try again"
    )
  }
}
