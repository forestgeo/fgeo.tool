#' Factory of functions to detect multiple values of a variable by groups.
#' 
#' Extends fgeo.base::multiple_var().
#' 
#' @param .data 
#'
#' @return
#' @export
#'
#' @examples
multiple_var_by_group <- function(var) {
  force(var)
  function(.data) {
    by_group <- tidyr::nest(.data)$data
    any(purrr::map_lgl(by_group, ~fgeo.base::multiple_var(var)(.x)))
  }
}

multiple_censusid <- fgeo.base::multiple_var("censusid")

multiple_plotname <- fgeo.base::multiple_var("plotname")
