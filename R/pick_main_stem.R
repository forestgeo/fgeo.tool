


#' Pick the main stem or main stemid(s) of each tree in each census.
#' 
#' * [pick_main_stem()] picks a unique row for each `treeID` per census.
#' * [pick_main_stemid()] picks a unique row for each `stemID` per census. It is
#' only useful when a single stem was measured twice in the same census, which
#' sometimes happens to correct for the effect of large buttresses.
#' 
#' * [pick_main_stem()] picks the main stem of each tree in each census. It
#' collapses data of multi-stem trees by picking a single stem per `treeid` per
#' `censusid`. From this group, it picks the stem at the top of a list sorted
#' first by descending order of `hom` and then by descending order of `dbh`.
#' This this corrects the effect of buttresses and picks the main stem. It
#' ignores groups of grouped data and rejects data with multiple plots.
#' * [pick_main_stemid()] does one step less than [pick_main_stem()]. It only
#' picks the main stemid(s) of each tree in each census and keeps all
#' stems per treeid. This is useful when calculating the total basal area of a
#' tree, because you need to sum the basal area of each individual stem as well as
#' sum only one of the potentially multiple measurements of each buttressed
#' stem per census.
#' 
#' @section Warning:
#' These functions may be considerably slow. They are fastest if the data
#' already has a single stem per treeid. They are slower with data containing
#' multiple stems per `treeid` (per `censusid`), which is the main reason for
#' using this function. The slowest scenario is when data also contains
#' duplicated values of `stemid` per `treeid` (per `censusid`). This may
#' happen if trees have buttresses, in which case these functions check
#' every stem for potential duplicates and pick the one with the largest `hom`
#' value. 
#' 
#' For example, in a windows computer with 32 GB of RAM, a dataset with 2
#' million rows with multiple stems and buttresses took about 3 minutes to run.
#' And a dataset with 2 million rows made up entirely of main stems took about
#' ten seconds to run.
#'
#' @template .x_fgeo
#'
#' @return A dataframe with a single plotname, and one row per per treeid per
#'   censusid.
#'
#' @examples
#' # One `treeID` with multiple stems. 
#' # `stemID == 1.1` has two measurements (due to buttresses).
#' # `stemID == 1.2` has a single measurement.
#' census <- tribble(
#'     ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~CensusID,
#'   "sp1",     "1",   "1.1",   140,   40,         1,  # main stemID (max `hom`)
#'   "sp1",     "1",   "1.1",   130,   60,         1,  
#'   "sp1",     "1",   "1.2",   130,   55,         1   # main stemID (only one)
#' )
#' 
#' # Picks a unique row per unique `treeID`
#' pick_main_stem(census)
#' 
#' # Picks a unique row per unique `stemID`
#' pick_main_stemid(census)
#'
#' @family functions to pick or drop rows of a ForestGEO dataframe
#' @name pick_main_stem
NULL

pick_main_f <- function(stemid = TRUE, treeid = TRUE) {
  function(.x) {
    stopifnot(is.data.frame(.x))
    
    # Store original row order to restore it at the end
    .x <- tibble::rowid_to_column(.x)
    # Lowercase names and groups to work with both census and ViewFullTable
    .data <- rlang::set_names(.x, tolower)
    # The net effect is to ignore groups: Store them now and restore them on exit.
    .data <- groups_lower(.data)
    
    stopifnot_single_plotname(.data)
    check_crucial_names(.data, c( "treeid", "stemid", "hom", "dbh"))
    
    .data <- pick_stemid_treeid(.data, stemid = stemid, treeid = treeid)
    
    # Restore rows order
    .data <- select(arrange(.data, .data$rowid), -.data$rowid)
    # Restore original names
    out <- rename_matches(.data , .x)
    # Restore original groups
    groups_restore(out, .x)
  }
}

#' @rdname pick_main_stem
#' @export
pick_main_stem <- pick_main_f(stemid = TRUE, treeid = TRUE)

#' @rdname pick_main_stem
#' @export
pick_main_stemid <- pick_main_f(stemid = TRUE, treeid = FALSE)

pick_stemid_treeid <- function(.data, stemid = TRUE, treeid = TRUE) {
  if (stemid) {
    .data <- pick_by_groups_by_censusid(.data, .data$treeid, .data$stemid)
  }
  if (treeid) {
    .data <- pick_by_groups_by_censusid(.data, .data$treeid)
  }
  .data
}

pick_by_groups_by_censusid <- function(.x, ...) {
  .x <- ungroup(.x)
  
  if (has_name(.x, "censusid") && detect_if(.x, "censusid", is_multiple)) {
    .x <- drop_if_na(.x, "censusid")
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
