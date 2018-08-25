#' Define the status of a tree based on the status of its stems.
#'
#' @template x_fgeo
#' @param status_a,status_d Sting to match alive and dead stems; it corresponds
#'   to the values of the variable `status` (in census tables) or `Status` (with
#'   capital "S" in ViewFull tables).
#'   
#' @family functions to add columns to dataframes.
#' @family functions for ForestGEO data.
#' @family functions for fgeo census.
#' @family functions for fgeo vft.
#'
#' @return The input data set with the additional variable `status_tree`.
#' @export
#'
#' @examples
#' library(fgeo.tool)
#' 
#' stem <- tibble::tribble(
#'   ~CensusID, ~treeID, ~stemID, ~status,
#'   1,       1,       1,     "A",
#'   1,       1,       2,     "D",
#'   # -- -- -- -- -- -- -- -- -- 
#'   1,       2,       3,     "D",
#'   1,       2,       4,     "D",
#'   # == == == == == == == == ==
#'   2,       1,       1,     "A",
#'   2,       1,       2,     "G",
#'   # -- -- -- -- -- -- -- -- -- 
#'   2,       2,       3,     "D",
#'   2,       2,       4,     "G"
#' )
#' 
#' # Determine the status of each tree based on the status of its stems
#' add_status_tree(stem)
add_status_tree <- function(x, status_a = "A", status_d = "D") {
  set_names(x, tolower) %>% 
    check_add_status_tree(status_a = status_a, status_d = status_d) %>% 
    group_by(.data$censusid, .data$treeid) %>% 
    mutate(
      status_tree = ifelse(all(.data$status == status_d), status_d, status_a)
    ) %>% 
    ungroup() %>% 
    fgeo.base::rename_matches(x)
}


check_add_status_tree <- function(x, status_d, status_a) {
  stopifnot(is.data.frame(x))
  check_crucial_names(x, c("treeid", "status", "censusid"))
  check_valid_status(x, .status = c(status_d, status_a), "status")
  if ("plotid" %in% names(x)) {
    msg <-  "\n  * Filter your data to keep a single plot and try again"
    flag_multiple_f("plotid", abort)(x, msg = msg)
  }
  invisible(x)
}

# TODO: Remove?
flag_multiple_f <- function(name, condition) {
  function(.data, msg) flag_if(.data, name, is_multiple, msg)
}
