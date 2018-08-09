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
#' if (requireNamespace("fgeo.map")) {
#'   library(fgeo.map)
#'   plot(hab1)
#' }
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
fgeo_habitat <- function(elevation,
                         gridsize,
                         n,
                         only_elev = FALSE,
                         edgecorrect = TRUE,
                         ...) {
  UseMethod("fgeo_habitat")
}

#' @export
fgeo_habitat.default <- function(elevation, gridsize, n, ...) {
  abort_bad_class(elevation)
}

#' @export
#' @rdname fgeo_habitat
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

#' @export
#' @rdname fgeo_habitat
fgeo_habitat.data.frame <- function(elevation,
                                    gridsize,
                                    n,
                                    xdim,
                                    ydim,
                                    only_elev = FALSE,
                                    edgecorrect = TRUE,
                                    ...) {
  if (missing(xdim) || missing(ydim)) {
    stop(
      "`xdim` and `ydim` can't be missing if `elevation` is a data.frame.", 
      call. = FALSE
    )
  }
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
                                 edgecorrect = TRUE) {
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
