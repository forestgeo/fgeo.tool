#' Path to directory containing example data.
#' 
#' @param path Path to a file (with extension) from inst/extdata/.
#'
#' @return Path to directory containing example data.
#' @export
#'
#' @examples
#' example_path_dir("two_files/new_stem_1.xlsx")
example_path_dir <- fgeo.base::example_path_factory("fgeo.tool", dirname)
