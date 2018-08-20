#' Pick the main stem of each tree in each census.
#' 
#' This function picks the main stem of each tree in each census. 
#' 
#' This function picks the main stem of each tree in each census. It collapses
#' data of multi-stem trees by picking a single stem per `treeid` per
#' `censusid`: Within this groups it picks the stem at the top of a list sorted
#' first by descending order of `hom`, and then by descending order of `dbh` --
#' this corrects the effect of buttresses and picks the main stem. It
#' intentionally rejects data with multiple plots, and ignores groups of grouped
#' data.
#' 
#' @section Warning:
#' This function may be considerably slow. It is fastest if the data already has
#' a single stem per treeid. It is slower if it detects multiple stems per
#' `treeid` (per `censusid`) -- which is the main reason for using this
#' function. It is slowest if it also duplicated values of `stemid` per `treeid`
#' (per `censusid`) -- which may happen if trees have buttresses -- in which
#' case, this function will check every stem for potential duplicates and pick
#' the one with the largest `hom` value.
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
#' library(tibble)
#' 
#' # Trees with buttresses may have more than one measurements per stem.
#' census <- tribble(
#'     ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~CensusID,
#'   "sp1",     "1",   "1.1",   140,   40,         1,  # main stem
#'   "sp1",     "1",   "1.1",   130,   60,         1,  
#'   "sp1",     "1",   "1.2",   130,   55,         1,  
#'   "sp2",     "2",   "2.1",   130,    5,         1   # main stem
#' )
#' 
#' # Piks largest hom first (to correct effect of batreesses) then largest dbh
#' pick_main_stem(census)
pick_main_stem <- function(.x) {
  stopifnot(is.data.frame(.x))
  stopifnot_single_plotname(.x)
  
  # Store original row order to restore it at the end
  .x <- tibble::rowid_to_column(.x)
  # Lowercase names and groups to work with both census and ViewFullTable
  .data <- rlang::set_names(.x, tolower)
  .data <- groups_lower(.data)
  
  fgeo.base::check_crucial_names(.data, c( "treeid", "stemid", "hom", "dbh"))
  .data <- pick_by_groups_by_censusid(.data, treeid, stemid)
  .data <- pick_by_groups_by_censusid(.data, treeid)
  
  # Restore rows order
  .data <- select(arrange(.data, .data$rowid), -.data$rowid)
  # Restore original names
  out <- fgeo.base::rename_matches(.data , .x)
  # Restore original groups
  groups_restore(out, .x)
}

pick_by_groups_by_censusid <- function(.x, ...) {
  .x <- ungroup(.x)
  
  if (multiple_censusid(.x)) {
    .x <- fgeo.base::drop_if_na(.x, "censusid")
    .x <- group_by(.x, .data$censusid)
  }
  
  grouped <- group_by(.x, !!! enquos(...), add = TRUE)
  
  not_duplicated <- !any(count(grouped)$n > 1)
  if (not_duplicated) return(ungroup(grouped))
  
  main_stems_at_top <- arrange(
    grouped, desc(.data$hom), desc(.data$dbh), .by_group = TRUE
  )
  main_stems <- filter(main_stems_at_top, dplyr::row_number() == 1L)
  ungroup(main_stems)
}
