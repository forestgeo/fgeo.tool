#' Create elevation data.
#'
#' This function constructs an object of class "fgeo_elevation". It standardizes
#' the structure of elevation data to always output a dataframe with names `gx`,
#' `gy` and `elev`.
#'
#' @param x One of these:
#'  * A dataframe containing elevation data, with columns `gx` and `gy` or `x`
#'  and `y` (e.g. `fgeo.x::elevation$col`).
#'  * A ForestGEO-like elevation list with elements
#'  `xdim` and `ydim` giving plot dimensions, and element `col` containing a
#'  dataframe as described in the previous item (e.g. `fgeo.x::elevation`).
#'
#' @return A dataframe with names `x/gx`, `y/gy` and `elev`.
#'
#' @section Acknowledgments:
#'   This function was inspired by David Kenfack.
#'
#' @examples
#' # Input: Elevation dataframe
#' elevation_df <- fgeo.x::elevation$col
#' fgeo_elevation(elevation_df)
#' 
#' class(elevation_df)
#' class(fgeo_elevation(elevation_df))
#' 
#' names(elevation_df)
#' names(fgeo_elevation(elevation_df))
#' 
#' # Input: Elevation list
#' elevation_ls <- fgeo.x::elevation
#' fgeo_elevation(elevation_ls)
#' 
#' class(elevation_ls)
#' class(fgeo_elevation(elevation_ls))
#' 
#' names(elevation_ls)
#' names(fgeo_elevation(elevation_ls))
#' @family functions to construct fgeo classes
#' @family habitat functions
#' @export
fgeo_elevation <- function(x) {
  UseMethod("fgeo_elevation")
}

#' @export
fgeo_elevation.fgeo_elevation <- function(x) {
  x
}

#' @export
fgeo_elevation.default <- function(x) {
  abort(glue("Can't deal with data of class {class(x)}"))
}

#' @export
fgeo_elevation.list <- function(x) {
  pull_elevation(x) %>%
    nms_try_rename(want = "gx", try = "x") %>%
    nms_try_rename(want = "gy", try = "y") %>%
    new_fgeo_elevation()
}

#' @export
fgeo_elevation.data.frame <- fgeo_elevation.list

new_fgeo_elevation <- function(x) {
  stopifnot(is.data.frame(x))
  structure(x, class = c("fgeo_elevation", class(x)))
}

pull_elevation <- function(x) {
  UseMethod("pull_elevation")
}

pull_elevation.data.frame <- function(x) {
  check_crucial_names(x, "elev")
  x
}

pull_elevation.default <- function(x) {
  msg <- paste0(
    "`elevation` must be data.frame or list but its class is: ", class(x)
  )
  abort(msg)
}

pull_elevation.list <- function(x) {
  safe_check <- purrr::safely(check_crucial_names)
  check_result <- safe_check(x, "col")
  if (!is.null(check_result$error)) {
    msg <- paste0(
      "Your list must contain the element `col` with elevation data.\n",
      "* Names of the elements of the list provided:\n",
      commas(names(x))
    )
    abort(msg)
  }

  elevation <- x[["col"]]
  elevation
}
