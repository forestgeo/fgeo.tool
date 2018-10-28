#' Print all rows and/or columns of a tibble.
#'
#' @inheritParams tibble::format.tbl
#'
#' @family general miscellaneous functions
#'
#' @return Print output (and the main input invisibly).
#'
#' @export
#' @examples
#' print(as_tibble(iris), n = 1)
#' print(as_tibble(iris), n = 100)
#' print(fgeo.data::luquillo_tree5_random)
#' print(fgeo.data::luquillo_tree5_random, n_extra = 2)
#' print(fgeo.data::luquillo_tree5_random, width = Inf)
print.tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  cli::cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @export
#' @rdname print.tbl
print.tbl_df <- print.tbl
