#' Assert a package is installed.
#'
#' @param pkg Character vector giving the name of a package.
#'
#' @return An error if `pkg` is not installed or invisible `pkg` if it is.
#' @export
#'
#' @examples
#' assert_is_installed("base")
#' try(assert_is_installed("bad"))
#' @keywords internal
assert_is_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    abort(glue("Please install the {pkg} package."))
  }
  invisible(pkg)
}
