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
#' add_status_tree(x)
add_status_tree <- function(x, status_d = "dead", status_a = "alive") {

  old <- names(x)
  x <- rlang::set_names(x, tolower)
  check_add_status_tree(x, status_d = status_d, status_a = status_a)

  grp <- group_by(x, .data$censusid, .data$tag)
  mut <- mutate(
    grp, status_tree = ifelse(
      all(.data$status == status_d),
      status_d,
      status_a
    )
  )
  restore_names(ungroup(mut), "status_tree", old)
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
