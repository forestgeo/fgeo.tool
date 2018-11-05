# TODO: Test

#' Read ForestGEO censuses.
#' 
#' This function reads any number of ForestGEO censuses stored in .rdata files
#' and stores them in a nested dataframe of class 'censuses_df', which has a
#' convenient print method and enables using specific methods for working with
#' ForestGEO data.
#' 
#' @seealso [rdata_df()], [as_censuses()].
#'
#' @inheritParams rdata_df
#' 
#' @family functions to import ForestGEO data
#'
#' @return An object of class 'censuses_df'.
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' censuses <- read_censuses(tool_example("rdata"))
#' class(censuses)
#' censuses$data
#' 
#' censuses %>% 
#'   pick(dbh > 600) %>% 
#'   pull(data)
read_censuses <- function(path_dir, .match = NULL, .id = "censuses") {
  dfm <- rdata_df(path_dir = path_dir, .match = .match, .id = .id)
  as_censuses(tidyr::nest(dfm, -.id))
}

#' Create objects of class `censuses_*`, where `*` depends on the input.
#'
#' @param .data A ForestGEO-like dataset.
#' 
#' @family functions to construct fgeo classes
#'
#' @return An object of class `census_*`, where `*` depends on the input.
#' @export
#' @keywords internal
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

#' @export
#' @rdname as_censuses
as_censuses.tbl_df <- function(.data) {
  new_censuses_df(.data)
}

#' @export
#' @rdname as_censuses
censuses_lst <- function(.data) {
  UseMethod("censuses_lst")
}

#' @export
#' @rdname as_censuses
censuses_df <- function(.data) {
  UseMethod("censuses_df")
}

new_censuses_lst <- function(x) {
  stopifnot(is.list(x))
  structure(x, class = c("censuses_lst", class(x)))
}

new_censuses_df <- function(x) {
  stopifnot(tibble::is.tibble(x))
  structure(x, class = c("censuses_df", class(x)))
}

#' @export
#' @noRd
print.censuses_lst <- function(x, ...) {
  print(unclass(x))
  invisible(x)
}
