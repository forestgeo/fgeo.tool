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
#' example_path("two_files/new_stem_1.xlsx")
#' dirname(example_path("two_files/new_stem_1.xlsx"))
#' basename(example_path("two_files/new_stem_1.xlsx"))
example_path <- function(path) {
  system.file("extdata", path, package = "fgeo.tool")
}
