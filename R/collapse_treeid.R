#' Pick only the one stem per treeid per censusid.
#' 
#' `collapse_treeid_max()` and `collapse_treeid_min()` pick the stem with the 
#' maximum and minimum dbh per treeid per censusid.
#'
#' @param .x A dataframe; particularly a ForestGEO census or ViewFullTable.
#'
#' @return A dataframe with one row per per treeid per censusid.
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
#' collapse_treeid_max(census)
#' collapse_treeid_min(census)
#' @name collapse_treeid
NULL

collapse_treeid <- function(.arrange) {
  force(.arrange)
  function(.x) {
    stopifnot(is.data.frame(.x))
    # Lowercase names and groups for work with both census and ViewFullTable
    .data <- rlang::set_names(.x, tolower)
    .data <- dplyr::grouped_df(.data, tolower(dplyr::group_vars(.data)))
    
    fgeo.base::check_crucial_names(.data, c("treeid", "dbh"))
    .data <- collapse_treeid_impl(.data, .arrange)
    
    # Restore original names; then original groups
    out <- fgeo.base::rename_matches(.data , .x)
    dplyr::grouped_df(out, dplyr::group_vars(.x))
  }
}

#' @rdname collapse_treeid
#' @export
collapse_treeid_min <- collapse_treeid(.arrange = identity)

#' @rdname collapse_treeid
#' @export
collapse_treeid_max <- collapse_treeid(.arrange = dplyr::desc)

collapse_treeid_impl <- function(x, .arrange) {
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

