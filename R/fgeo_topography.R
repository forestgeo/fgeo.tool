#' Calculate mean elevation, convexity and slope.
#' 
#' @inheritSection fgeo_elevation Input
#' 
#' @param elevation One of these:
#'  * A list with at least three elements: `col` containing
#'  elevation data; and `xdim` and `ydim` giving plot dimensions; OR
#'  * A dataframe containing elevation data, in which
#'  case the parameters `xdim` and `ydim` must be provided. It may be the 
#'  element `col` of a ForestGEO elevation-list or an object of class 
#'  fgeo_elevation (see [fgeo_elevation()]).
#' @param gridsize Number giving the size of each quadrat for which a habitat
#'   is calculated. Commonly, `gridsize = 20`.
#' @param xdim,ydim (If `elevation` is a dataframe) `x` and `y` dimensions of
#'   the plot.
#' @param edgecorrect Correct convexity in edge quadrats?
#' @param ... Other arguments passed to methods.
#' 
#' @seealso [fgeo_habitat()].
#' 
#' @inherit fgeo_habitat details
#'
#' @return A dataframe of subclass fgeo_topography.
#'
#' @export
#' 
#' @examples
#' elev_list <- fgeo.data::luquillo_elevation
#' fgeo_topography(elev_list, gridsize = 20)
#' 
#' elev_df <- elev_list$col
#' fgeo_topography(elev_df, gridsize = 20, xdim = 320, ydim = 500)
fgeo_topography <- function(elevation, ...) {
  UseMethod("fgeo_topography")
}

#' @export
fgeo_topography.default <- function(elevation, gridsize, ...) {
  abort_bad_class(elevation)
}

#' @rdname fgeo_topography
#' @export
fgeo_topography.data.frame <- function(elevation, 
                                       gridsize, 
                                       xdim = NULL,
                                       ydim = NULL,
                                       edgecorrect = TRUE,
                                       ...) {
  force(gridsize)
  abort_if_xdim_ydim_is_null(xdim, ydim)
  
  elevation_ls <- list(col = elevation, xdim = xdim, ydim = ydim)
  fgeo_topography.list(elevation = elevation_ls, gridsize, edgecorrect)
}

#' @rdname fgeo_topography
#' @export
fgeo_topography.list <- function(elevation, 
                                 gridsize, 
                                 edgecorrect = TRUE, 
                                 ...) {
  force(gridsize)
  plotdim <- c(elevation$xdim, elevation$ydim)
  
  # Match names-requirements of allquadratslopes()
  names(elevation$col) <- sub("gx", "x", names(elevation$col))
  names(elevation$col) <- sub("gy", "y", names(elevation$col))
  topo <- suppressMessages(
    allquadratslopes(elevation, gridsize, plotdim, edgecorrect)
  )
  
  quad_idx <- as.integer(rownames(topo))
  gxgy <- index_to_gxgy(quad_idx, gridsize, plotdim)
  out <- tibble::as.tibble(cbind(gxgy, topo))
  new_fgeo_topography(out)
}

new_fgeo_topography <- function(x) {
  structure(x, class = c("fgeo_topography", class(x)))
}

abort_if_xdim_ydim_is_null <- function(xdim, ydim) {
  msg <- "`xdim` and `ydim` can't be `NULL` if `elevation` is a data.frame."
  xdim %||% abort(msg)
  ydim %||% abort(msg)
}
