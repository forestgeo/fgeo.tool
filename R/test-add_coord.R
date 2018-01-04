#' Add coordinates (of a ForestGEO plot) to a dataframe.
#'
#' These functions wrap legacy code from the [CTFS R
#' Package](http://ctfs.si.edu/Public/CTFSRPackage/).
#'
#' @name convert_coordinates
NULL

#' @examples
library(dplyr)
x <- tibble(
  gx = 100:102,
  gy = 100:102
)

add_coord <- function(x, from, to, gridsize = NULL, plotdim = NULL) {
  if (is.null(gridsize)) {gridsize <- guess_gridsize(x)}
  if (is.null(gridsize)) {plotdim <- guess_plotdim(x)}

  stopifnot(is.data.frame(x))
  stopifnot(is.character(from), is.character(to))

  if (all(from == "gxgy", to == "lxly")) {
    check_crucial_names(x, c("gx", "gy"))
    lxly <- gxgy.to.lxly(x$gx, x$gy, gridsize = gridsize, plotdim = plotdim)
    tibble::add_column(x, lx = lxly$lx, ly = lxly$ly)
  }
  # xxx cont here.
}

guess_gridsize <- function(x) {
  # xxx
}
guess_plotdim <- function(x) {
  # xxx
}
