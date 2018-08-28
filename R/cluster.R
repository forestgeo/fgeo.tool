# FIXME: DOCUMENT

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
