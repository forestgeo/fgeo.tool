#' @export
as_censuses <- function(.data, ...) {
  UseMethod("as_censuses")
}

#' @export
as_censuses.default <- function(.data, ...) {
  abort_bad_class(.data)
}

#' @export
as_censuses.list <- function(.data, ...) {
  new_censuses_lst(.data)
}

#' @export
as_censuses.tbl_df <- function(.data, ...) {
  new_censuses_tbl(.data)
}

#' @export
censuses_lst <- function(.data, ...) {
  UseMethod("censuses_lst")
}

#' @export
censuses_tbl <- function(.data, ...) {
  UseMethod("censuses_tbl")
}

new_censuses_lst <- function(x) {
  stopifnot(is.list(x))
  structure(x, class = c("censuses_lst", class(x)))
}

new_censuses_tbl <- function(x) {
  stopifnot(tibble::is.tibble(x))
  structure(x, class = c("censuses_tbl", class(x)))
}

print.censuses_lst <- function (x, ...) {
  print(unclass(x))
  invisible(x)
}
