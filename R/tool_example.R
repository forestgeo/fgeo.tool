#' Path to directory containing example data.
#' 
#' @param path Path to a file (with extension) from inst/extdata/.
#'
#' @return Path to directory containing example data.
#' @export
#'
#' @export
#' 
#' @examples
#' tool_example("two_files/new_stem_1.xlsx")
#' dirname(tool_example("two_files/new_stem_1.xlsx"))
#' basename(tool_example("two_files/new_stem_1.xlsx"))
tool_example <- function(path) {
  system.file("extdata", path, package = "fgeo.tool")
}
