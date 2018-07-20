# fgeo.tool ---------------------------------------------------------------

#' Factory of functions to pick woods and then apply a function.
#'
#' @param .f A function to apply after picking woods. For example, `.f =
#'   identity` returns the picked woods unchanged.
#'
#' @keywords internal
#' @export
#'
#' @examples
#' 
#' @noRd
pick_woods_f <- function(.f, .collapse = fgeo.tool::pick_dbh_largest) {
  force(.f)
  
  function(.data, ...) {
    stopifnot(is.data.frame(.data))
    # Lowercase names and groups for work with both census and ViewFullTable
    .x <- set_names(.data, tolower)
    .x <- groups_lower(.x)
    
    if (multiple_plotname(.x)) {
      stop("`.x` must have a single plotname.", call. = FALSE)
    }
    
    if (multiple_censusid(.x)) {
      .x <- fgeo.base::drop_if_na(.x, "censusid")
      .x <- dplyr::group_by(.x, .data$censusid, add = TRUE)
    }
    
    # do() prefferred to by_group() to not drop empty groups (they result in 0L)
    dots <- rlang::enquos(...)
    out <- dplyr::do(
      .x, picked_woods_f_impl(., !!! dots, .collapse = .collapse, .f = .f)
    )
    
    # Restore original names; then original groups
    out <- rename_matches(out, .data)
    groups_restore(out, .data)
  }
}

#' Pick woods after collapsing multiple stems by treeid and censusid.
#' 
#' @description 
#' These functions pick rows by groups. At the core they run `dplyr::filter()`
#' but include these additional features:
#' * They reject multiple plots with an informative error message.
#' * They operate within groups define with `dplyr::group_by()`.
#' * If the data has multiple censuses, they automatically group by census.
#' * They collapse data of multi-stem trees by picking a single stem per treeid.
#' The picked stem depends on the values of `hom` or `dbh` detected for each 
#' census of each treeid. The selection is as follows:
#' FIXME: Not yet implemented
#'   * If there is a single `hom` value, it is the stem of maximum `dbh`.
#'   * If there is more than one `hom` value, it is the stem of maximum `dbh`.
#' 
#' @description
#' * `pick_woods()` is a general function that takes any dataframe and any 
#' number of expressions to filter the dataframe.
#' * `pick_trees()` picks stems of 100 mm dbh and above.
#' * `pick_saplings()` picks stems between 10 mm dbh inclusive and 100 mm dbh 
#' exclusive.
#' 
#' 
#' @export
#' @rdname pick_woods
pick_woods <- pick_woods_f(identity, .collapse = fgeo.tool::pick_dbh_largest)

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

multiple_plotname <- fgeo.base::multiple_var("plotname")

multiple_censusid <- fgeo.base::multiple_var("censusid")

picked_woods_f_impl <- function(.data, ..., .collapse, .f) {
  .dots <- rlang::enquos(...)
  pick <- dplyr::filter( .collapse(.data), !!! .dots)
  .f(pick)
}
