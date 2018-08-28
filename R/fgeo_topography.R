#' Measure topography and apply hierarchical clustering.
#' 
#' These functions overlap, but -- depending on the context -- you may choose
#' one or the other to more clearly communicate your intention:
#' * `fgeo_topography()` calculates mean elevation, convexity and slope.
#' * `cluster_elevation()` outputs one additional column, `cluster`, calculated
#' by hierarchical clustering of the topographic metrics calculated by
#' `fgeo_topography()`. `cluster_elevation()` first calculates a
#' dissimilarities object (with [stats::dist()] and all its defaults), then it 
#' calculates a tree (with [stats::hclust()] and all its defaults), and finally
#' cuts the tree in `n` groups (with [stats::cutree()]).
#' 
#' @inheritParams construct_habitats
#' @seealso [fgeo_habitat()].
#' 
#' @inherit construct_habitats details
#'
#' @return A dataframe.
#'
#' @examples
#' elev_ls <- fgeo.data::luquillo_elevation
#' fgeo_topography(elev_ls, gridsize = 20)
#' cluster_elevation(elev_ls, gridsize = 20, n = 4)
#' 
#' elev_df <- elev_ls$col
#' fgeo_topography(elev_df, gridsize = 20, xdim = 320, ydim = 500)
#' cluster_elevation(elev_df, gridsize = 20, n = 4 , xdim = 320, ydim = 500)
#' 
#' # To decide the value of `n` you may inspect the dendrogram of topography.
#' topo <- fgeo_topography(elev_ls, gridsize = 20)
#' topo
#' topo_vars <- c("meanelev", "convex", "slope")
#' plot(hclust(dist(topo[topo_vars])))
#' @name topography_metrics
#' @aliases fgeo_topography cluster_elevation
NULL

#' @export
fgeo_topography <- function(elevation, ...) {
  UseMethod("fgeo_topography")
}

#' @export
fgeo_topography.default <- function(elevation, gridsize, ...) {
  abort_bad_class(elevation)
}

#' @rdname topography_metrics
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

#' @rdname topography_metrics
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




# Cluster -----------------------------------------------------------------


#' @export
cluster <- function(.data, ...) {
  UseMethod("cluster")
}

#' @export
cluster.default <- function(.data, ...) {
  abort_bad_class(.data)
}

#' @export
cluster.fgeo_topography <- function(.data, n) {
  if (!is.numeric(n)) abort("`n` must be numeric")
  
  cluster_vars <- c("meanelev", "convex", "slope")
  .data$cluster <- withr::with_seed(1, 
    stats::cutree(stats::hclust(stats::dist(.data[cluster_vars])), n)
  )
  .data
}

















#' @export
cluster_elevation <- function(elevation, ...) {
  UseMethod("cluster_elevation")
}

#' @export
cluster_elevation.default <- function(elevation, ...) {
  abort_bad_class(elevation)
}

#' @rdname topography_metrics
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

#' @rdname topography_metrics
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

