#' Table functions by package.
#' 
#' @inheritParams functionMap::map_r_package
#'
#' @return A tibble.
#' @author Based on code by Noam Ross (@noamross), developed during runconf18
#' [https://github.com/ropensci/unconf18].
#' 
#' @family functions for developers.
#' 
#' @examples
#' \dontrun{
#' funs_by_package()
#' }
#' @noRd
funs_by_package <- function(path = ".", include_base = FALSE) {
  if (identical(path, ".")) {
    path <- usethis::proj_get()
  }
  
  functionMap::map_r_package(path, include_base)$node_df %>% 
    dplyr::filter(ID != "::") %>% 
    tidyr::separate(ID, into = c("package", "fn"), sep = "::") %>% 
    dplyr::mutate(
      fn = ifelse(own, basename(package), fn),
      package = ifelse(own, basename(path), package)
    ) %>% 
    dplyr::as_tibble()
}



#' Given a function and paths to packages, find packages that use the function.
#'
#' @param paths A string giving a full path to packages directories.
#' @param fn A string giving a function name.
#'
#' @author Based on code by Noam Ross (@noamross), developed during runconf18
#' [https://github.com/ropensci/unconf18].
#' 
#' @return A tibble.
#'
#' @family functions for developers.
#' 
#' @examples
#' \dontrun{
#' library(dplyr)
#' pkgs <- fgeo::fgeo_index_functions() %>% 
#'   as_tibble() %>% 
#'   pull(package) %>% 
#'   unique()
#' dir <- fs::path_dir(usethis::proj_get())
#' paths <- fs::path(dir, pkgs)
#' 
#' funs_table <- find_users(paths, "check_crucial_names")
#' funs_table
#' }
#' @noRd
find_users <- function(paths, fn) {
  all_funs <- purr::map_df(paths, funs_by_package)
  dplyr::filter(all_funs, fn == fn)
}
