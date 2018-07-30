#' Construct habitat data.
#'
#' This function constructs an object of class fgeo_habitat. It takes either the
#' elevation list that ForestGEO delivers, or its element `col` -- which
#' contains the elevation data. Notice that the required arguments to
#' `fgeo_habitat()` vary according to the main input (the elevation list or the
#' elevation dataframe).
#' 
#' @param elevation
#'   * A list with at least three elements: `col` containing
#'   elevation data; and `xdim` and `ydim` giving plot dimensions; OR
#'   * A dataframe containing elevation data, in which
#'   case the parameters `xdim` and `ydim` must be provided. It may be the 
#'   element `col` of a ForestGEO elevation-list or an object of class 
#'   fgeo_elevation (see [fgeo_elevation()]).
#' @param gridsize Number to round `x` and `y` by. Commonly, `gridsize = 20`.
#' @param n Number of elevation groups (habitats) to cut elevation data by. 
#' Commonly, `n = 4`.
#' @param xdim,ydim `x` and `y` dimensions of the plot.
#' @param ... Other arguments passed to methods.
#'
#' @return A dataframe of habitat data, with columns `gx` and `gy`, rounded
#'   with accuracy determined by `gridsize`, and column `habitats`, with as 
#'   many distinct values as determined by the argument `n`. 
#' @export
#'
#' @examples
#' # Input: Object of class list
#' elev_list <- fgeo.data::luquillo_elevation
#' hab1 <- fgeo_habitat(elev_list, gridsize = 20, n = 4)
#' str(hab1)
#' 
#' # Input: Object of class dataframe
#' elev_df <- fgeo.data::luquillo_elevation$col
#' hab2 <- fgeo_habitat(elev_df, gridsize = 20, n = 4, xdim = 320, ydim = 500)
#' str(hab2)
#' 
#' # Input: Object of class fgeo_elev
#' fgeo_elev <- fgeo_elevation(fgeo.data::luquillo_elevation)
#' hab3 <- fgeo_habitat(elev_df, gridsize = 20, n = 4, xdim = 320, ydim = 500)
#' str(hab3)
#' 
#' identical(hab1, hab2)
#' identical(hab2, hab3)
#' hab1
fgeo_habitat <- function(elevation, gridsize, n, ...) {
  UseMethod("fgeo_habitat")
}

#' @export
fgeo_habitat.default <- function(elevation, gridsize, n, ...) {
  abort(paste0("Can't deal with data of class ", class(elevation)))
}

#' @export
#' @rdname fgeo_habitat
fgeo_habitat.list <- function(elevation, gridsize, n, ...) {
  check_crucial_names(elevation, c("col", "xdim", "ydim"))
  fgeo_habitat.data.frame(
    elevation$col, gridsize, n, elevation$xdim, elevation$ydim
  )
}

#' @export
#' @rdname fgeo_habitat
fgeo_habitat.data.frame <- function(elevation, gridsize, n, xdim, ydim, ...) {
  if (missing(xdim) || missing(ydim)) {
    stop(
      "`xdim` and `ydim` can't be missing if `elevation` is a data.frame.", 
      call. = FALSE
    )
  }
  elevation_to_habitat(fgeo_elevation(elevation), gridsize, n, xdim, ydim)
}

elevation_to_habitat <- function(elevation, gridsize, n, xdim, ydim) {
  fgeo.tool::fgeo_elevation(elevation) %>%
    dplyr::as_tibble() %>% 
    dplyr::mutate(
      x = fgeo.base::round_any(.data$gx, gridsize),
      y = fgeo.base::round_any(.data$gy, gridsize)
    ) %>% 
    unique() %>% 
    dplyr::group_by(.data$gx, .data$gy) %>%
    dplyr::summarise(elev = mean(.data$elev)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      gx = as.integer(fgeo.base::round_any(.data$gx, accuracy = gridsize)),
      gy = as.integer(fgeo.base::round_any(.data$gy, accuracy = gridsize)),
      habitats = as.integer(cut_number(.data$elev, n)), 
      elev = NULL
    ) %>% 
    unique() %>% 
    dplyr::filter(.data$gx < xdim, .data$gy < ydim) %>% 
    new_fgeo_habitat()
}

new_fgeo_habitat <- function(x) {
  structure(x, class = c("fgeo_habitat", class(x)))
}
