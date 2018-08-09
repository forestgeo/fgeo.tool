#' Construct habitats from measures of topography (or only elevation).
#'
#' @description 
#' This function constructs habitat data based on elevation data. It calculates
#' habitats in two steps:
#' 1. It calculates mean elevation, convexity and slope for each quadrat (via
#' [measure_topography()])).
#' 2. It calculates habitas based on the topographic metrics from step 1:
#'     * If `only_elev = FALSE` (default) habitats are calculated by applying 
#'     [stats::kmeans()] clustering on all three topographic metrics from step 1.
#'     (For an output that shows all three topographic metrics plus the 
#'     resulting cluster, use [cluster_elevation()]).
#'     * If `only_elev = TRUE` habitats are calculated by applying [base::cut()]
#'     on the mean elevation from step 1 (ignoring convexity and slope).
#'
#' @description 
#' The input can be either the elevation list that ForestGEO delivers, or the
#' element `col` of such list -- which is a dataframe containing the elevation
#' data. Notice that the required arguments to `fgeo_habitat()` vary according
#' to the main input (the elevation list or the elevation dataframe).
#' 
#' @description 
#' The outputs an object of class fgeo_habitat, which you can visualize directly
#' with `plot()` (assuming you are using __fgeo.map__, for example via 
#' `library(fgeo)`). 
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
#' @param xdim,ydim `x` and `y` dimensions of the plot.
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
#' @export
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
#'   
#'   # Compare
#'   hab2 <- fgeo_habitat(elev_list, gridsize = 20, n = 4, only_elev = TRUE)
#'   plot(hab2)
#' }
#' 
#' # Input: Object of class dataframe
#' elev_df <- fgeo.data::luquillo_elevation$col
#' hab2 <- fgeo_habitat(elev_df, gridsize = 20, n = 4, xdim = 320, ydim = 500)
#' str(hab2)
fgeo_habitat <- function(elevation,
                         gridsize,
                         n,
                         xdim = NULL,
                         ydim = NULL,
                         only_elev = FALSE,
                         edgecorrect = TRUE,
                         ...) {
  UseMethod("fgeo_habitat")
}

fgeo_habitat.default <- function(elevation, ...) {
  abort_bad_class(elevation)
}

fgeo_habitat.list <- function(elevation,
                              gridsize,
                              n,
                              only_elev = FALSE,
                              edgecorrect = TRUE,
                              ...) {
  check_crucial_names(elevation, c("col", "xdim", "ydim"))
  fgeo_habitat.data.frame(
    elevation$col, gridsize, n, elevation$xdim, elevation$ydim, 
    only_elev, edgecorrect
  )
}

fgeo_habitat.data.frame <- function(elevation,
                                    gridsize,
                                    n,
                                    xdim = NULL,
                                    ydim = NULL,
                                    only_elev = FALSE,
                                    edgecorrect = TRUE,
                                    ...) {
  msg <- "`xdim` and `ydim` can't be `NULL` if `elevation` is a data.frame."
  xdim %||% abort(msg)
  ydim %||% abort(msg)
  
  elevation_to_habitat(
    fgeo_elevation(elevation), gridsize, n, xdim, ydim, only_elev, edgecorrect
  )
}

elevation_to_habitat <- function(elevation,
                                 gridsize,
                                 n,
                                 xdim,
                                 ydim,
                                 only_elev,
                                 edgecorrect) {
  elev_ls <- list(col = elevation, xdim = xdim, ydim = ydim)
  out <- cluster_elevation(elev_ls, gridsize, n, edgecorrect)
  if (only_elev) {
    out$cluster <- as.integer(cut(out$meanelev, n, 1:n))
  }
  
  names(out) <- sub("cluster", "habitats", names(out))
  out <- out[c("gx", "gy", "habitats")]
  new_fgeo_habitat(out)
}

new_fgeo_habitat <- function(x) {
  structure(x, class = c("fgeo_habitat", class(x)))
}

#' @export
measure_topography <- function(elevation, gridsize, ...) {
  UseMethod("measure_topography")
}

#' @export
measure_topography.default <- function(elevation, gridsize, ...) {
  abort_bad_class(elevation)
}

#' @export
measure_topography.data.frame <- function(elevation, 
                                          gridsize, 
                                          xdim,
                                          ydim,
                                          edgecorrect = TRUE) {
  force(gridsize)
  elevation_ls <- list(col = elevation, xdim = xdim, ydim = ydim)
  measure_topography.list(elevation = elevation_ls, gridsize, edgecorrect)
}

#' @export
measure_topography.list <- function(elevation, gridsize, edgecorrect = TRUE) {
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

#' @export
cluster_elevation <- function(elevation, gridsize, n, ...) {
  UseMethod("cluster_elevation")
}

#' @export
cluster_elevation.default <- function(elevation, gridsize, ...) {
  abort_bad_class(elevation)
}

#' @export
cluster_elevation.list <- function(elevation, gridsize, n, edgecorrect = TRUE) {
  force(gridsize)
  force(n)
  
  # Ensure kmeans() returns always the same result
  old_seed <- get(".Random.seed", .GlobalEnv)
  on.exit(assign(".Randon.seed", old_seed, .GlobalEnv))
  set.seed(1)
  
  hab <- measure_topography.list(elevation, gridsize, edgecorrect = edgecorrect)
  cluster_vars <- c("meanelev", "convex", "slope")
  hab$cluster <- stats::kmeans(hab[cluster_vars], n)$cluster
  hab
}

#' @export
cluster_elevation.data.frame <- function(elevation,
                                         gridsize,
                                         n,
                                         xdim,
                                         ydim,
                                         edgecorrect = TRUE) {
  force(gridsize)
  force(n)
  
  elevation_ls <- list(col = elevation, xdim = xdim, ydim = ydim)
  cluster_elevation.list(elevation_ls, gridsize, n, edgecorrect)
}

abort_bad_class <- function(x) {
  abort(glue("Can't deal with data of class {class(x)}."))
}
