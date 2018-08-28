#' Calculate mean elevation, convexity and slope.
#' 
#' @inheritParams fgeo_habitat
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





# FIXME: Remove

# Cluster -----------------------------------------------------------------

#' @export
cluster_elevation <- function(elevation, ...) {
  UseMethod("cluster_elevation")
}

#' @export
cluster_elevation.default <- function(elevation, ...) {
  abort_bad_class(elevation)
}

#' @rdname fgeo_topography
#' @export
cluster_elevation.list <- function(elevation, 
                                   gridsize, 
                                   n, 
                                   only_elev = FALSE,
                                   edgecorrect = TRUE,
                                   ...) {
  force(gridsize)
  force(n)
  
  hab <- fgeo_topography.list(elevation, gridsize, edgecorrect = edgecorrect)
  cluster_vars <- c("meanelev", "convex", "slope")
  if (only_elev) cluster_vars <- c("meanelev")
  
  hab$cluster <- withr::with_seed(1, 
    stats::cutree(stats::hclust(stats::dist(hab[cluster_vars])), n)
  )
  hab
}

#' @rdname fgeo_topography
#' @export
cluster_elevation.data.frame <- function(elevation,
  gridsize,
  n,
  xdim = NULL,
  ydim = NULL,
  only_elev = FALSE,
  edgecorrect = TRUE,
  ...) {
  force(gridsize)
  force(n)
  abort_if_xdim_ydim_is_null(xdim, ydim)
  
  elevation_ls <- list(col = elevation, xdim = xdim, ydim = ydim)
  cluster_elevation.list(elevation_ls, gridsize, n, only_elev, edgecorrect)
}

