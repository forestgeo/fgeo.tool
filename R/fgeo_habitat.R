#' Construct habitats from measures of topography (or only elevation).
#'
#' This function constructs habitat data based on elevation data. It calculates
#' habitats in two steps:
#' 1. It calculates mean elevation, convexity and slope for each quadrat (via
#' [measure_topography()])).
#' 2. It calculates habitats based on the topographic metrics from step 1:
#'     * If `only_elev = FALSE` (default) habitats are calculated by applying 
#'     [stats::kmeans()] clustering on all three topographic metrics from step 1.
#'     (For an output that shows all three topographic metrics plus the 
#'     resulting cluster, use [cluster_elevation()]).
#'     * If `only_elev = TRUE` habitats are calculated by applying
#'     [stats::kmeans()] only to the mean elevation from step 1 (ignoring
#'     convexity and slope).
#'
#' The input can be either the elevation list that ForestGEO delivers, or the
#' element `col` of such list -- which is a dataframe containing the elevation
#' data. Notice that the required arguments to `fgeo_habitat()` vary according
#' to the main input (the elevation list or the elevation dataframe).
#' 
#' @seealso [fgeo.map::plot.fgeo_habitat()], [measure_topography()],
#'   [cluster_elevation()].
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
#' @param n Number of distinct habitat-categories to construct. 
#' @param xdim,ydim (If `elevation` is a dataframe) `x` and `y` dimensions of
#'   the plot.
#' @param only_elev Should the clusters be calculated using only elevation?
#' * If `FALSE` (default) habitats are calculated by applying
#' [stats::kmeans()] clustering on all three topographic metrics from step 1.
#' * If `TRUE` habitats are calculated by applying [base::cut()] on the mean
#' elevation from step 1 (ignoring convexity and slope).
#' @param edgecorrect Correct convexity in edge quadrats?
#' @param ... Other arguments passed to methods.
#'
#' @return A dataframe of subclass fgeo_habitat, with columns `gx` and `gy`
#'   rounded with accuracy determined by `gridsize`, and column `habitats`, with
#'   as many distinct integer values as determined by the argument `n`.
#'
#' @examples
#' # Input: Object of class list
#' elev_list <- fgeo.data::luquillo_elevation
#' hab1 <- fgeo_habitat(elev_list, gridsize = 20, n = 4)
#' str(hab1)
#' 
#' if (requireNamespace("fgeo.map")) {
#'   library(fgeo.map)
#'   plot(hab1)
#' }
#' 
#' # Compare
#' if (requireNamespace("fgeo.map")) {
#'   library(fgeo.map)
#'   
#'   hab2 <- fgeo_habitat(elev_list, gridsize = 20, n = 4, only_elev = TRUE)
#'   plot(hab2)
#' }
#' 
#' # A good use of habitat data is for calculating species-habitat associations
#' if (requireNamespace("fgeo.habitat")) {
#'   library(fgeo.habitat)
#'   
#'   elev_list <- fgeo.data::luquillo_elevation
#'   habitat <- fgeo_habitat(elev_list, gridsize = 20, n = 4)
#'   census <- fgeo.habitat::luquillo_top3_sp
#'   species <- unique(census$sp)
#'   to_df(tt_test(census, species, habitat))
#' }
#' 
#' # If working with elevation dataframe you must provide xdim and ydim
#' elev_df <- fgeo.data::luquillo_elevation$col
#' hab2 <- fgeo_habitat(elev_df, gridsize = 20, n = 4, xdim = 320, ydim = 500)
#' str(hab2)
#' @name construct_habitats
#' @aliases fgeo_habitat
NULL

#' @export
fgeo_habitat <- function(elevation, ...) {
  UseMethod("fgeo_habitat")
}

#' @export
fgeo_habitat.default <- function(elevation, ...) {
  abort_bad_class(elevation)
}

#' @rdname construct_habitats
#' @export
fgeo_habitat.list <- function(elevation,
                              gridsize,
                              n,
                              only_elev = FALSE,
                              edgecorrect = TRUE,
                              ...) {
  check_crucial_names(elevation, c("col", "xdim", "ydim"))
  fgeo_habitat.data.frame(
    elevation = elevation$col, 
    gridsize = gridsize, 
    n = n, 
    xdim = elevation$xdim, 
    ydim = elevation$ydim, 
    only_elev = only_elev, 
    edgecorrect = edgecorrect
  )
}

#' @rdname construct_habitats
#' @export
fgeo_habitat.data.frame <- function(elevation,
                                    gridsize,
                                    n,
                                    xdim = NULL,
                                    ydim = NULL,
                                    only_elev = FALSE,
                                    edgecorrect = TRUE,
                                    ...) {
  abort_if_xdim_ydim_is_null(xdim, ydim)
  
  elevation_to_habitat(
    fgeo_elevation(elevation), gridsize, n, xdim, ydim, only_elev, edgecorrect
  )
}

elevation_to_habitat <- function(elevation,
                                 gridsize,
                                 n,
                                 xdim,
                                 ydim,
                                 only_elev = FALSE,
                                 edgecorrect) {
  elev_ls <- list(col = elevation, xdim = xdim, ydim = ydim)
  out <- cluster_elevation(elev_ls, gridsize, n, only_elev, edgecorrect)
  names(out) <- sub("cluster", "habitats", names(out))
  out <- out[c("gx", "gy", "habitats")]
  new_fgeo_habitat(out)
}

#' Measure topography and apply `kmeans()` clustering.
#' 
#' These functions overlap, but -- depending on the context -- you may choose
#' one or the other to more clearly communicate your intention:
#' * `measure_topography()` calculates mean elevation, convexity and slope.
#' * `cluster_elevation()` outputs one additional column, `cluster`, calculated
#' by applying [stats::kmeans()] on the topographic metrics calculated by
#' `measure_topography()`. 
#' 
#' FIXME: NEW OPTION TO CLUSTER WITH 
#' d <- dist(d, method = "euclidean")
#' hclust(d, method = "complete")
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
#' measure_topography(elev_ls, gridsize = 20)
#' cluster_elevation(elev_ls, gridsize = 20, n = 4)
#' 
#' elev_df <- elev_ls$col
#' measure_topography(elev_df, gridsize = 20, xdim = 320, ydim = 500)
#' cluster_elevation(elev_df, gridsize = 20, n = 4 , xdim = 320, ydim = 500)
#' @name topography_metrics
#' @aliases measure_topography cluster_elevation
NULL

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
  
  hab <- measure_topography.list(elevation, gridsize, edgecorrect = edgecorrect)
  cluster_vars <- c("meanelev", "convex", "slope")
  if (only_elev) cluster_vars <- c("meanelev")
  x <- hab[cluster_vars]
  hab$cluster <- withr::with_seed(1, cluster(x, n, use_kmeans = TRUE))
  hab
}

cluster <- function(x, n, use_kmeans = TRUE) {
  if (use_kmeans) return(stats::kmeans(x, n)$cluster)
  stats::cutree(stats::hclust(stats::dist(x)), n)
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
#' @export
measure_topography <- function(elevation, ...) {
  UseMethod("measure_topography")
}

#' @export
measure_topography.default <- function(elevation, gridsize, ...) {
  abort_bad_class(elevation)
}

#' @rdname topography_metrics
#' @export
measure_topography.data.frame <- function(elevation, 
                                          gridsize, 
                                          xdim = NULL,
                                          ydim = NULL,
                                          edgecorrect = TRUE,
                                          ...) {
  force(gridsize)
  abort_if_xdim_ydim_is_null(xdim, ydim)
  
  elevation_ls <- list(col = elevation, xdim = xdim, ydim = ydim)
  measure_topography.list(elevation = elevation_ls, gridsize, edgecorrect)
}

#' @rdname topography_metrics
#' @export
measure_topography.list <- function(elevation, 
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
  tibble::as.tibble(cbind(gxgy, topo))
}

new_fgeo_habitat <- function(x) {
  structure(x, class = c("fgeo_habitat", class(x)))
}

abort_bad_class <- function(x) {
  abort(glue("Can't deal with data of class {class(x)}."))
}

abort_if_xdim_ydim_is_null <- function(xdim, ydim) {
  msg <- "`xdim` and `ydim` can't be `NULL` if `elevation` is a data.frame."
  xdim %||% abort(msg)
  ydim %||% abort(msg)
}
