#' Factory of functions to pick woods and then apply a function.
#'
#' @param .f A function to apply after picking woods. For example, `.f =
#'   identity` returns the picked woods unchanged.
#' @param .collapse A function to collapse multiple values of `treeID` into a 
#'   single one.
#'
#' @export
pick_woods_f <- function(.f, .collapse = fgeo.tool::pick_largest_hom_dbh) {
  force(.f)
  
  function(.data, ...) {
    stopifnot(is.data.frame(.data))
    # Lowercase names and groups for work with both census and ViewFullTable
    .x <- set_names(.data, tolower)
    .x <- groups_lower(.x)
    
    stopifnot_single_plotname(.x)
    
    if (multiple_censusid(.x)) {
      .x <- fgeo.base::drop_if_na(.x, "censusid")
      .x <- dplyr::group_by(.x, .data$censusid, add = TRUE)
    }
    
    # do() prefferred to by_group() to not drop empty groups (they result in 0L)
    dots <- rlang::enquos(...)
    out <- dplyr::do(
      .x, pick_woods_f_impl(., !!! dots, .collapse = .collapse, .f = .f)
    )
    
    # Restore original names; then original groups
    out <- rename_matches(out, .data)
    groups_restore(out, .data)
  }
}

#' Pick main stems within a specific dbh range.
#' 
#' @description 
#' 
#' * It rejects multiple plots with an informative error message.
#' * If the data has multiple censuses, it automatically group by census.
#' * They collapse data of multi-stem trees by picking a single stem per
#' `treeid` per `censusid`: Within this groups they pick the stem at the top of
#' a list sorted first by descending order of `hom`, and then by descending
#' order of `dbh` -- this corrects the effect of buttresses and picks the main
#' stem.
#' 
#' These functions pick rows by groups. At the core they run `dplyr::filter()`
#' but include these additional features:
#' * They operate within groups defined with `dplyr::group_by()`.
#' 
#' @description
#' `pick_woods()` is a general function that picks rows of a dataframe based on
#' any number of expressions. The other functions are shortcuts:
#' * `pick_trees()` picks stems of 100 mm dbh and above.
#' * `pick_saplings()` picks stems between 10 mm dbh inclusive and 100 mm dbh 
#' exclusive.
#' * `pick_saplings_and_trees()` picks stems of 10 mm dbh and above.
#' 
#' @param .data A ForestGEO-like dataframe, either a census or ViewFullTable.
#' @param ... Expressions passed to `dplyr::filter()`, E.g. the 
#'   expression `dbh >= 100` picks `dbh` values of 100 mm and above.
#'
#' @family functions for fgeo census and vft.
#' 
#' @return A dataframe. See description.
#' 
#' @export
#' 
#' @examples 
#' census <- tibble::tribble(
#'     ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~CensusID,
#'   "sp1",     "1",   "1.2",   130,  122,         1,
#'   "sp1",     "1",   "1.1",   130,   10,         1,
#'   "sp2",     "2",   "2.1",   130,   22,         1,
#'   "sp2",     "2",   "2.2",   130,   99,         1,
#'   "sp2",     "2",   "2.2",   144,   88,         1,
#'   "sp2",     "2",   "2.3",   130,   NA,         1,
#'                                        
#'   "sp1",     "1",   "1.2",   130,  123,         2,
#'   "sp1",     "1",   "1.1",   130,   11,         2,
#'   "sp2",     "2",   "2.1",   130,   22,         2,
#'   "sp2",     "2",   "2.2",   130,  110,         2,
#'   "sp2",     "2",   "2.2",   144,  101,         2,
#'   "sp2",     "2",   "2.3",   130,   NA,         2
#' )
#' 
#' # Piks largest hom first (to correct effect of batreesses) then largest dbh
#' pick_woods(census, dbh >=10)
#' 
#' pick_woods(census, dbh >=10, dbh < 100)
#' # Same
#' pick_saplings(census)
#' 
#' pick_trees(census)
#' 
#' pick_saplings_and_trees(census)
pick_woods <- pick_woods_f(
  identity, .collapse = fgeo.tool::pick_largest_hom_dbh
)

#' @export
#' @rdname pick_woods
pick_trees <- function(.data) {
  pick_woods(.data, .data$dbh >= 100)
}

#' @export
#' @rdname pick_woods
pick_saplings <- function(.data) {
  pick_woods(.data, .data$dbh >= 10, .data$dbh < 100)
}

#' @export
#' @rdname pick_woods
pick_saplings_and_trees <- function(.data) {
  pick_woods(.data, .data$dbh >= 10)
}

multiple_plotname <- fgeo.base::multiple_var("plotname")

multiple_censusid <- fgeo.base::multiple_var("censusid")

pick_woods_f_impl <- function(.data, ..., .collapse, .f) {
  .dots <- rlang::enquos(...)
  pick <- dplyr::filter( .collapse(.data), !!! .dots)
  .f(pick)
}
