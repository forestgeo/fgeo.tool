#' Path to directory containing example data.
#' 
#' @param path Path to a file (with extension) from inst/extdata/.
#' 
#' @keywords internal
#'
#' @return Path to directory containing example data.
#' 
#' @export
#' @examples
#' tool_example("csv")
#' dir(tool_example("csv"))
#' csv_list(tool_example("csv"))
tool_example <- function (path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "fgeo.tool"))
  }
  else {
    system.file("extdata", path, package = "fgeo.tool", mustWork = TRUE)
  }
}
