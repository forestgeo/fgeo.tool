#' Pick the (stem of) largest dbh per treeid per censusid.
#' 
#' This function picks the stem with the maximum dbh per treeid per censusid. It
#' intentionally errs if it detects multiple distinct values of plotname (i.e.
#' if the data is a ViewFullTable with multiple plots).
#'
#' @param .x A ForestGEO-like dataframe, census or ViewFullTable.
#'
#' @return A dataframe with one row per per treeid per censusid and a single 
#'   plotname.
#' 
#' @family functions to pick or drop rows of a dataframe.
#'
#' @export
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
#' pick_dbh_largest(census)
pick_dbh_largest <- function(.x) {
  stopifnot(is.data.frame(.x))
  
  # Lowercase names and groups for work with both census and ViewFullTable
  .data <- rlang::set_names(.x, tolower)
  .data <- groups_lower(.data)
  
  fgeo.base::check_crucial_names(.data, c("stemid", "hom"))
  .data <- pick_hom_by_stemid_by_treeid_censusid(
    .data, .arrange = dplyr::desc, .pick = dplyr::row_number() == 1L
  )
  
  fgeo.base::check_crucial_names(.data, c("treeid", "dbh"))
  .data <- pick_dbh_by_treeid_by_censusid(
    .data, .arrange = dplyr::desc, .pick = dplyr::row_number() == 1L
  )
  
  # Restore original names; then original groups
  out <- fgeo.base::rename_matches(.data , .x)
  groups_restore(out, .x)
}

#' Implementation of pick_dbh_largest.
#' 
#' Name is intentionally different. It's less concise but more descriptive.
#' 
#' @param .arrange Function to control arrangement of data within each group:
#'   E.g. `identity()` or `dplyr::desc()`.
#' @param .pick Expression to control what rows to pick within each group: E.g.
#'   `row_number() == 1L` or `dplyr::between(row_number(), 1, 10)`.
#' @noRd
pick_dbh_by_treeid_by_censusid <- function(x, .arrange, .pick) {
  .pick <- enquo(.pick)
  # Grouping must be handleded at higher levels.
  .x <- dplyr::ungroup(x)
  
  stopifnot_single_plotname(.x)
  
  if (multiple_censusid(.x)) {
    .x <- fgeo.base::drop_if_na(.x, "censusid")
    .x <- dplyr::group_by(.x, .data$censusid)
  }
  
  .x %>%
    dplyr::group_by(.data$treeid, add = TRUE) %>% 
    dplyr::arrange(.arrange(.data$dbh)) %>%
    # row_number() can be used with single table verbs without specifying x
    dplyr::filter(!! .pick) %>% 
    dplyr::ungroup()
}

# FIXME: DRY with function just above
pick_hom_by_stemid_by_treeid_censusid <- function(x, .arrange, .pick) {
  .pick <- enquo(.pick)
  # Grouping must be handleded at higher levels.
  .x <- dplyr::ungroup(x)
  
  stopifnot_single_plotname(.x)
  
  if (multiple_censusid(.x)) {
    .x <- fgeo.base::drop_if_na(.x, "censusid")
    .x <- dplyr::group_by(.x, .data$censusid)
  }
  
  .x %>%
    dplyr::group_by(.data$treeid, .data$stemid, add = TRUE) %>% 
    dplyr::arrange(.arrange(.data$hom), .arrange(.data$dbh)) %>%
    # row_number() can be used with single table verbs without specifying x
    dplyr::filter(!! .pick) %>% 
    dplyr::ungroup()
}
