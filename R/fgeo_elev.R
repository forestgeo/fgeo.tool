#' Construct elevation data.
#' 
#' This function constructs an object of class fgeo_elevation. It standardizes
#' the structure of elevation data to always output a dataframe with names `gx`,
#' `gy` and `elev` -- or it dies trying.
#' 
#' The input may be a dataframe or a dataframe stored in the element `col` of a
#' list; the column names of the dataframe may be `gx` and `gy` or `x` and `y`.
#' 
#' @param x Either a dataframe or a dataframe stored in the element `col` of a
#'   list.
#'
#' @return A dataframe with names `x/gx`, `y/gy` and `elev`.
#' 
#' @section Acknowledgments:
#'   This function was inspired by David Kenfack.
#' 
#' @export
#'
#' @examples
#' # Dataframe input
#' elev_df <- bciex::bci_elevation
#' str(elev_df)
#' 
#' elev <- fgeo_elevation(elev_df)
#' str(elev)
#' 
#' # List input
#' elev_list <- list(col = elev_df, other = "stuff")
#' str(elev_list)
#' 
#' elev <- fgeo_elevation(elev_list)
#' str(elev) 
fgeo_elevation <- function(x) {
  UseMethod("fgeo_elevation")
}

#' @export
fgeo_elevation.fgeo_elevation <- function(x) {
  x
}

#' @export
fgeo_elevation.default <- function(x) {
  abort(paste0("Can't deal with data of class ", class(x)))
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
  fgeo.base::check_crucial_names(x, "elev")
  x
}

pull_elevation.default <- function(x) {
  msg <- paste0(
    "`elevation` must be data.frame or list but its class is: ", class(x)
  )
  rlang::abort(msg)
}

pull_elevation.list <- function(x) {
  safe_check <- purrr::safely(fgeo.base::check_crucial_names)
  check_result <- safe_check(x, "col")
  if (!is.null(check_result$error)) {
    
    msg <- paste0(
      "Your list must contain the element `col` with elevation data.\n",
      "* Names of the elements of the list provided:\n",
      commas(names(x))
    )
    rlang::abort(msg)
  }
  
  elevation <- x[["col"]]
  elevation
}
