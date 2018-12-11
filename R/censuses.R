#' Create objects of class `censuses_*`, where `*` depends on the input.
#'
#' @param .data A ForestGEO-like dataset.
#' 
#' @family functions to construct fgeo classes
#'
#' @return An object of class `census_*`, where `*` depends on the input.
#' @export
#'
#' @examples
#' censuses_lst <- as_censuses(list(
#'   c1 = tibble(dbh = 1:2),
#'   c2 = tibble(dbh = 8:9)
#' ))
#' class(censuses_lst)
as_censuses <- function(.data) {
  UseMethod("as_censuses")
}

#' @export
#' @rdname as_censuses
as_censuses.default <- function(.data) {
  abort_bad_class(.data)
}

#' @export
#' @rdname as_censuses
as_censuses.list <- function(.data) {
  new_censuses_lst(.data)
}

censuses_lst <- function(.data) {
  UseMethod("censuses_lst")
}

new_censuses_lst <- function(x) {
  stopifnot(is.list(x))
  structure(x, class = c("censuses_lst", class(x)))
}

#' @export
#' @noRd
print.censuses_lst <- function(x, ...) {
  print(unclass(x))
  invisible(x)
}

