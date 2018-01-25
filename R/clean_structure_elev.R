# Manipulate elevation ----------------------------------------------------

#' If necessary, pull elevation data and rename `x` and `y` to `gx` and `gy`.
#' 
#' This functions helps standarize elevation data. Elevation data may be stored
#' as a dataframe in the element `col` of a list or may be directly the
#' dataframe. Also, the names may be `x` and `y` wich is inconsistent with the
#' names `gx` and `gy` of census datasets. Whatever the structure of the input
#' (either a dataframe or a list) and whatever the names (`x` and `y`, or `gx`
#' and `gy`), this function outputs a dataframe with names `gx` and `gy`, and
#' checks that the variable `elev` is present.
#' 
#' @param elevation A dataframe or list of ForestGEO's elevation data.
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' # Dataframe input
#' elev_df <- bciex::bci_elevation
#' str(elev_df)
#' 
#' elev_clean <- restructure_elev(elev_df)
#' str(elev_clean)
#' 
#' # List input
#' elev_list <- list(col = elev_df, other = "stuff")
#' str(elev_list)
#' 
#' elev_clean <- restructure_elev(elev_list)
#' str(elev_clean) 
restructure_elev <- function(elevation) {
  pull_elevation(x = elevation) %>% 
    nms_try_rename(want = "gx", try = "x") %>% 
    nms_try_rename(want = "gy", try = "y")
}

pull_elevation <- function(x) {
  UseMethod("pull_elevation")
}

pull_elevation.data.frame <- function(x) {
  fgeo.tool::check_crucial_names(x, "elev")
  x
}

pull_elevation.default <- function(x) {
  msg <- paste0(
    "`elevation` must be data.frame or list but its class is: ", class(x)
  )
  rlang::abort(msg)
}

pull_elevation.list <- function(x) {
  safe_check <- purrr::safely(fgeo.tool::check_crucial_names)
  check_result <- safe_check(x, "col")
  if (!is.null(check_result$error)) {
    
    msg <- paste0(
      "Your list must contain the element `col` with elevation data.\n",
      "* Names of the elements of the list provided:\n",
      collapse(names(x))
    )
    rlang::abort(msg)
  }
  
  elevation <- x[["col"]]
  elevation
}

nms_try_rename <- function(x, want, try) {
  nm <- fgeo.tool::nms_extract1(x = x, want = want, try = try)
  if (length(nm) == 0) {
    rlang::abort(
      paste0("Data must have a column named `", want, "` or `", try, "`")
    )
  }
  names(x)[grepl(nm, names(x))] <- want
  x
}
