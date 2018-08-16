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
#' These functions extend `pick_largest_hom_dbh()`. Working with simple or
#' grouped dataframes (via `dplyr::group_by()`) these functions let you pick
#' main stems within a given `dbh` range:
#' * `pick_woods()` is a general function and lets you choose the `dbh` range. The
#' other functions are shortcuts with pre-defined `dbh` ranges:
#'   * `pick_saplings_and_trees()` picks stems of 10 mm dbh and above.
#'   * `pick_saplings()` picks stems between 10 mm dbh inclusive and 100 mm dbh
#'   exclusive.
#'   * `pick_trees()` picks stems of 100 mm dbh and above.
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
#' library(tibble)
#' 
#' census <- tribble(
#'     ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~CensusID,
#'   "sp1",     "1",   "1.1",   140,   40,         1,  # main stem
#'   "sp1",     "1",   "1.1",   130,   60,         1   # buttress
#' )
#' 
#' # Trees with buttresses may have more than one measurements per stem.
#' # Piks largest hom first (to correct effect of batreesses) then largest dbh
#' pick_largest_hom_dbh(census)
#' 
#' # Also works with multiple censuses
#' censuses <- tribble(
#'     ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~CensusID,
#'   "sp1",     "1",   "1.1",   140,   40,         1,  # main stem
#'   "sp1",     "1",   "1.1",   130,   60,         1,  # buttress
#'   "sp1",     "1",   "1.1",   140,   50,         2,  # main stem
#'   "sp1",     "1",   "1.1",   130,   70,         2   # buttress
#' )
#' 
#' pick_largest_hom_dbh(censuses)
#' 
#' # pick_woods() and friends internally pick the main stem, then pick by dbh
#' pick_woods(censuses)
#' 
#' pick_woods(censuses, dbh > 40)
#' 
#' # These functions are convenient shortcuts
#' census <- tribble(
#'     ~sp, ~treeID, ~stemID,  ~hom, ~dbh, ~CensusID,
#'   "sp1",     "1",   "1.1",   140,   40,         1,  # main stem
#'   "sp1",     "1",   "1.1",   130,   60,         1,  # buttress
#'   "sp1",     "2",   "2.1",   130,   100,        1,
#'   "sp1",     "3",   "2.1",   130,   110,        1,
#' )
#' 
#' pick_saplings(census)
#' pick_trees(census)
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
