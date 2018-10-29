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
#' tool_example("multiple_workbooks")
#' dir(tool_example("multiple_workbooks"))
#' xl_df(tool_example("multiple_workbooks"))
tool_example <- function(path) {
  system.file("extdata", path, package = "fgeo.tool")
}
