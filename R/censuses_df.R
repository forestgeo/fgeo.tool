#' @export
#' @rdname as_censuses
as_censuses.tbl_df <- function(.data) {
  new_censuses_df(.data)
}
new_censuses_df <- function(x) {
  stopifnot(tibble::is.tibble(x))
  structure(x, class = c("censuses_df", class(x)))
}

censuses_df <- function(.data) {
  UseMethod("censuses_df")
}
