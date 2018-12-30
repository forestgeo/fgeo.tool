#' Add column `status_tree` based on the status of all stems of each tree.
#'
#' @template x_fgeo
#' @param status_a,status_d Sting to match alive and dead stems; it corresponds
#'   to the values of the variable `status` (in census tables) or `Status` (with
#'   capital "S" in ViewFull tables).
#'   
#'
#' @return The input data set with the additional variable `status_tree`.
#'
#' @examples
#' stem <- tribble(
#'   ~CensusID, ~treeID, ~stemID, ~status,
#'           1,       1,       1,     "A",
#'           1,       1,       2,     "D",
#'           # -- -- -- -- -- -- -- -- -- 
#'           1,       2,       3,     "D",
#'           1,       2,       4,     "D",
#'           # == == == == == == == == ==
#'           2,       1,       1,     "A",
#'           2,       1,       2,     "G",
#'           # -- -- -- -- -- -- -- -- -- 
#'           2,       2,       3,     "D",
#'           2,       2,       4,     "G"
#' )
#' 
#' # Determine the status of each tree based on the status of its stems
#' add_status_tree(stem)
#' 
#' @family functions to add columns to dataframes
#' @family functions for ForestGEO data
#' @family functions for fgeo census
#' @family functions for fgeo vft
#' @export
add_status_tree <- function(x, status_a = "A", status_d = "D") {
  set_names(x, tolower) %>% 
    check_add_status_tree(status_a = status_a, status_d = status_d) %>% 
    group_by(.data$censusid, .data$treeid) %>% 
    mutate(
      status_tree = ifelse(all(.data$status == status_d), status_d, status_a)
    ) %>% 
    ungroup() %>% 
    rename_matches(x)
}

check_add_status_tree <- function(x, status_d, status_a) {
  stopifnot(is.data.frame(x))
  check_crucial_names(x, c("treeid", "status", "censusid"))
  check_valid_status(x, .status = c(status_d, status_a), "status")
  if ("plotid" %in% names(x)) {
    msg <-  "\n  * Filter your data to keep a single plot and try again"
    flag_if(x, "plotid", is_multiple, abort, msg = msg)
  }
  invisible(x)
}

check_valid_status <- function(x, .status, status_var) {
  .status_var <- x[[status_var]]
  check_crucial_names(x, status_var)
  valid_status <- unique(.status_var)
  invalid_status <- setdiff(.status, valid_status)
  if (length(invalid_status) != 0) {
    warning(
      "No observation has .status = ", commas(invalid_status), "\n",
      "  * Detected values: ", commas(valid_status),
      call. = FALSE
    )
  }
}
