#' Find fgeo packages that depend on a given package.
#'
#' Useful to find which packages may have broken after refactoring on package
#' (the one you would want to pass to the argument `pkg`).
#'
#' @param pkg String giving the name of one package which dependencies you want
#'   to find.
#' @param root Sting. Assumes all fgeo package have the same root directory.
#' @param fgeo_pkgs Character string giving the names of fgeo packages.
#'
#' @family functions for developers.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' # Won't because it depends on local directories.
#' \dontrun{
#' fgeo_package_deps("fgeo.utils")
#' }
fgeo_package_deps <- function(pkg,
                              root = "../",
                              fgeo_pkgs = c(
                                "bciex",
                                "fgeo.abundance",
                                "fgeo.utils",
                                "map",
                                "fgeo"
                              )) {
  fgeo_deps <- list_fgeo_deps(root, fgeo_pkgs)
  deps_matching_pkg <- purrr::keep(fgeo_deps, ~any(grepl(pkg, .)))
  names(deps_matching_pkg)
}
list_fgeo_deps <- function(root, fgeo_pkgs) {
  paths <- paste0(root, fgeo_pkgs)
  all_deps <- purrr::map(paths, remotes::local_package_deps, TRUE)
  all_deps <- purrr::set_names(all_deps, fgeo_pkgs)
  fgeo_deps <- purrr::map(all_deps, ~intersect(., fgeo_pkgs))
  fgeo_deps
}
