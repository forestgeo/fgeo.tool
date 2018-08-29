#' Construct habitats from measures of topography (or only elevation).
#'
#' This function constructs habitat data based on elevation data. It calculates
#' habitats in two steps:
#' 1. It calculates mean elevation, convexity and slope for each quadrat (via
#' [fgeo_topography()])).
#' 2. It calculates habitats based on hierarchical clustering of the topographic
#' metrics from step 1 (via [add_cluster()]).
#'
#' The input can be either the elevation list that ForestGEO delivers, or the
#' element `col` of such list -- which is a dataframe containing the elevation
#' data. Notice that the required arguments to `fgeo_habitat()` vary according
#' to the main input (the elevation list or the elevation dataframe).
#' 
#' @seealso [fgeo.map::plot.fgeo_habitat()], [fgeo_topography()],
#'   [add_cluster()].
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
#' @param edgecorrect Correct convexity in edge quadrats?
#' @param ... Other arguments passed to methods.
#'
#' @return A dataframe of subclass fgeo_habitat, with columns `gx` and `gy`,
#'   rounded with accuracy determined by `gridsize`, and column `habitats`, with
#'   as many distinct integer values as determined by the argument `n`.
#'
#' @export
#' 
#' @examples
#' # Input a ForestGEO-like elevation list
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
#'   hab2 <- fgeo_habitat(elev_list, gridsize = 20, n = 4)
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
fgeo_habitat <- function(elevation, n, ...) {
  out <- add_cluster(fgeo_topography(elevation, ...), n)
  names(out) <- sub("cluster", "habitats", names(out))
  new_fgeo_habitat(out[c("gx", "gy", "habitats")])
}


new_fgeo_habitat <- function(x) {
  structure(x, class = c("fgeo_habitat", class(x)))
}

