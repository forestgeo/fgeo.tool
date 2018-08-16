#' Pick the main stem of each tree in each census.
#' 
#' This function picks the main stem of each tree in each census. 
#' 
#' This function picks the main stem of each tree in each census. It
#' collapses data of multi-stem trees by picking a single stem per `treeid` per
#' `censusid`: Within this groups it picks the stem at the top of a list sorted
#' first by descending order of `hom`, and then by descending order of `dbh` --
#' this corrects the effect of buttresses and picks the main stem. It
#' intentionally rejects data with multiple plots, and ignores groups of grouped
#' data (to operate within groups see ?[pick_woods()]).
#'
#' @param .x A ForestGEO-like dataframe, census or ViewFullTable.
#' @seealso [pick_woods()].
#'
#' @return A dataframe with one row per per treeid per censusid and a single 
#'   plotname.
#' 
#' @family functions to pick or drop rows of a dataframe.
#'
#' @export
#'
#' @examples
#' library(tibble)
#' 
#' # Trees with buttresses may have more than one measurements per stem.
#' census <- tribble(
#'     ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~CensusID,
#'   "sp1",     "1",   "1.1",   140,   40,         1,  # main stem # FIXME
#'   "sp1",     "1",   "1.1",   130,   60,         1,  # buttress
#'   "sp1",     "1",   "1.2",   130,   55,         1,  
#'   "sp2",     "2",   "2.1",   130,    5,         1,
#' )
#' 
#' # Piks largest hom first (to correct effect of batreesses) then largest dbh
#' pick_largest_hom_dbh(census)
pick_largest_hom_dbh <- function(.x) {
  stopifnot(is.data.frame(.x))
  .x <- tibble::rowid_to_column(.x)
  
  # Lowercase names and groups for work with both census and ViewFullTable
  .data <- rlang::set_names(.x, tolower)
  .data <- groups_lower(.data)
  
  fgeo.base::check_crucial_names(.data, c("stemid", "hom"))
  .data <- pick_hom_by_stemid_by_treeid_censusid(.data)
  
  fgeo.base::check_crucial_names(.data, c("treeid", "dbh"))
  .data <- pick_dbh_by_treeid_by_censusid(.data)
  
  # Restore row order
  .data <- dplyr::select(dplyr::arrange(.data, .data$rowid), -.data$rowid)
  # Restore original names
  out <- fgeo.base::rename_matches(.data , .x)
  # Restore original groups
  groups_restore(out, .x)
}

pick_dbh_by_treeid_by_censusid <- function(x) {
  # Grouping must be handleded at higher levels.
  .x <- dplyr::ungroup(x)
  
  stopifnot_single_plotname(.x)
  
  if (multiple_censusid(.x)) {
    .x <- fgeo.base::drop_if_na(.x, "censusid")
    .x <- dplyr::group_by(.x, .data$censusid)
  }
  
  .x %>%
    dplyr::group_by(.data$treeid, add = TRUE) %>% 
    # FIXME: Restore original order, by using original rownumber.
    dplyr::arrange(
      dplyr::desc(.data$hom), dplyr::desc(.data$dbh), .by_group = TRUE
    ) %>%
    # row_number() can be used with single table verbs without specifying x
    dplyr::filter(dplyr::row_number() == 1L) %>% 
    dplyr::ungroup()
}

# TODO: DRY with function just above
pick_hom_by_stemid_by_treeid_censusid <- function(x) {
  # Grouping must be handleded at higher levels.
  .x <- dplyr::ungroup(x)
  
  stopifnot_single_plotname(.x)
  
  if (multiple_censusid(.x)) {
    .x <- fgeo.base::drop_if_na(.x, "censusid")
    .x <- dplyr::group_by(.x, .data$censusid)
  }
  
  .x %>%
    dplyr::group_by(.data$treeid, .data$stemid, add = TRUE) %>% 
    dplyr::arrange(dplyr::desc(.data$hom), dplyr::desc(.data$dbh)) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>% 
    dplyr::ungroup()
}
