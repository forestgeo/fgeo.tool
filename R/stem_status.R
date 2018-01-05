#' Filter a data set by the status of its stems.
#'
#' @template x_fgeo
#' @param .status Character vector; Must be one of possible values of the
#'   variable giving the status of the dataframe `x`.
#'
#' @return
#' @export
#'
#' @examples
status_stem <- function(x, .status) {
  # Work with viewfull- and census-tables regardless names case
  old_nms <- names(x)
  x <- rlang::set_names(x, tolower)
  check_status_stem(x = x, .status = .status)

  filtered <- dplyr::filter(x,  .data$status  %in%  .status)
  rlang::set_names(filtered, old_nms)
}

# x <- bciex::bci12t7mini
# status_stem(x, .status = "D")

check_status_stem <- function(x, .status){
  stopifnot(is.data.frame(x))
  stopifnot(is.character(.status))
  check_crucial_names(x, "status")
  is_possible_status <- unique(x$status)
  stopifnot(.status %in% is_possible_status)
}
