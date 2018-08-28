#' Hierarchical clustering of topography metrics.
#' 
#' This functoin adds the column `cluster` to objects of class fgeo_topography.
#' Using the __stats__ package, `cluster()` calculates a hierarchical clustering
#' of the columns `meanelev`, `slope` and `convexity`. First, it calculates a
#' dissimilarities among those columns (with [stats::dist()] and all its
#' defaults), then it calculates a tree (with [stats::hclust()] and all its
#' defaults), and finally cuts the tree in `n` groups (with [stats::cutree()]).
#' 
#' @param .data A dataframe of subclass fgeo_topography.
#' @param n Integer. Number of cluster-groups to construct (passed to the
#'   argument `k` to [stats::cutree()]).
#'
#' @export
#' 
#' @examples 
#' elev_ls <- fgeo.data::luquillo_elevation
#' topo <- fgeo_topography(elev_ls, gridsize = 20)
#' cluster(topo, n = 4)
cluster <- function(.data, n) {
  invalid_class <- !any(grepl("fgeo_topography", class(.data)))
  if (invalid_class) abort_bad_class(.data)
  
  
  if (!is.numeric(n)) abort("`n` must be numeric")
  
  cluster_vars <- c("meanelev", "convex", "slope")
  .data$cluster <- withr::with_seed(1, 
    stats::cutree(stats::hclust(stats::dist(.data[cluster_vars])), n)
  )
  .data
}
