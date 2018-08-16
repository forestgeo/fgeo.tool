#' Pick the main stem of each tree in each census.
#' 
#' This function picks the main stem of each tree in each census. 
#' 
#' 
#' It
#' intentionally errs if it detects multiple plots. It ignores groups of grouped
#' data (to operate within groups see ?[pick_woods()]).
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
#'   ~dbh,   ~sp, ~treeID, ~stemID, ~hom,
#'     10, "sp1",     "1",   "1.1",   10,
#'    100, "sp1",     "1",   "1.2",   10,
#'     22, "sp2",     "2",   "2.1",   10,
#'     99, "sp2",     "2",   "2.2",   10,
#'     99, "sp2",     "2",   "2.3",   10,
#'     NA, "sp2",     "2",   "2.4",   10
#' )
#' 
#' pick_largest_hom_dbh(census)
pick_largest_hom_dbh <- function(.x) {
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

#' Implementation of pick_largest_hom_dbh
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

# TODO: DRY with function just above
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
