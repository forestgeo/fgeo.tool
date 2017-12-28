#' Test if an object exists in the namespace of a package.
#'
#' Test if an object exists in the namespace of a package. Useful inside an
#' `if` statement.
#'
#' @param object String giving the object to find.
#' @param package String giving the package which namespace to search.
#'
#' @return Logical.
#' @export
#' @family functions for developers.
#' @examples
#' exists_in_pkg("cars", "datasets")
exists_in_pkg <- function(object, package){
  any(grepl(object, ls(paste0("package:", package))))
}
