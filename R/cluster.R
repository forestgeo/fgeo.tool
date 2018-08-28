#' * `cluster_elevation()` outputs one additional column, `cluster`, calculated
#' by hierarchical clustering of the topographic metrics calculated by
#' `fgeo_topography()`. `cluster_elevation()` first calculates a
#' dissimilarities object (with [stats::dist()] and all its defaults), then it 
#' calculates a tree (with [stats::hclust()] and all its defaults), and finally
#' cuts the tree in `n` groups (with [stats::cutree()]).
#' 
#' @param .data A dataframe.
#' @inheritParams n fgeo_habitat
#'
#' @param ... Other arguments passed to methods.
#'
#' @export
#' 
#' @examples 
#' elev_ls <- fgeo.data::luquillo_elevation
#' topo <- fgeo_topography(elev_ls, gridsize = 20)
#' cluster(topo, n = 4)
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
