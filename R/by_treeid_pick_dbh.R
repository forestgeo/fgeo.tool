#' Pick only the one stem per treeid (per censusid).
#' 
#' `by_treeid_pick_dbh_max()` and `by_treeid_pick_dbh_min()` pick the stem with the 
#' maximum and minimum dbh per treeid per censusid. It intentionally errs if
#' it detects multiple distinct values of plotname (i.e. if the data is a 
#' ViewFullTable).
#'
#' @param .x A dataframe; particularly a ForestGEO census or ViewFullTable.
#'
#' @return A dataframe with one row per per treeid per censusid and a single 
#'   plotname.
#' 
#' @family functions to pick or drop rows of a dataframe.
#'
#' @examples
#' census <- tibble::tribble(
#'   ~dbh,   ~sp, ~treeID, ~stemID,
#'     10, "sp1",     "1",   "1.1",
#'    100, "sp1",     "1",   "1.2",
#'     22, "sp2",     "2",   "2.1",
#'     99, "sp2",     "2",   "2.2",
#'     99, "sp2",     "2",   "2.3",
#'     NA, "sp2",     "2",   "2.4"
#' )
#' 
#' by_treeid_pick_dbh_max(census)
#' 
#' by_treeid_pick_dbh_min(census)
#' @name by_treeid_pick_dbh
NULL

by_treeid_pick_dbh <- function(.arrange) {
  force(.arrange)
  function(.x) {
    stopifnot(is.data.frame(.x))
    # Lowercase names and groups for work with both census and ViewFullTable
    .data <- rlang::set_names(.x, tolower)
    .data <- groups_lower(.data)
    
    fgeo.base::check_crucial_names(.data, c("treeid", "dbh"))
    .data <- by_treeid_pick_dbh_impl(.data, .arrange)
    
    # Restore original names; then original groups
    out <- fgeo.base::rename_matches(.data , .x)
    groups_restore(out, .x)
  }
}

#' @rdname by_treeid_pick_dbh
#' @export
by_treeid_pick_dbh_min <- by_treeid_pick_dbh(.arrange = identity)

#' @rdname by_treeid_pick_dbh
#' @export
by_treeid_pick_dbh_max <- by_treeid_pick_dbh(.arrange = dplyr::desc)

by_treeid_pick_dbh_impl <- function(x, .arrange) {
  # Grouping must be handleded at higher levels.
  .x <- dplyr::ungroup(x)
  
  if (multiple_plotname(.x)) {
    stop("`.x` must have a single plotname.", call. = FALSE)
  }
  
  if (multiple_censusid(.x)) {
    .x <- fgeo.base::drop_if_na(.x, "censusid")
    .x <- dplyr::group_by(.x, .data$censusid)
  }
  
  .x %>%
    dplyr::group_by(.data$treeid, add = TRUE) %>% 
    dplyr::arrange(.arrange(.data$dbh)) %>%
    dplyr::filter(dplyr::row_number() == 1) %>% 
    dplyr::ungroup()
}

