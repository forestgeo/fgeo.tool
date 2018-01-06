#' Filter a data set by the status of its stems.
#'
#' @template x_fgeo
#' @param .status Character vector; Must be one of possible values of the
#'   variable giving the status of the dataframe `x`.
#' @param exclude Logical; `TRUE` excluded the values passed to `.status`.
#'
#' @return A filtered version of the dataframe `x`.
#' @export
#'
#' @examples
#' x <- tibble::tibble(status = LETTERS[1:4])
#' x
#'
#' status_stem(x, "A")
#' status_stem(x, c("A", "C"))
#' status_stem(x, c("A", "C"), exclude = TRUE)
#'
#' status_stem(x, c("D"), exclude = TRUE)
#' # Shortcut
#' stem_not_dead(x)
status_stem <- function(x, .status,  exclude = FALSE) {
  # Work with viewfull- and census-tables regardless names case
  old_nms <- names(x)
  x <- rlang::set_names(x, tolower)
  check_status_stem(x = x, .status = .status)

  if (exclude) {
    not_status <- setdiff(unique(x$status), .status)
    filtered <- dplyr::filter(x, .data$status %in% not_status)
  } else {
    filtered <- dplyr::filter(x, .data$status %in% .status)
  }
  rlang::set_names(filtered, old_nms)
}

#' @rdname status_stem
#' @export
stem_not_dead <- function(x, .status = "D") {
  status_stem(x = x, .status = .status, exclude = TRUE)
}

check_status_stem <- function(x, .status){
  stopifnot(is.data.frame(x))
  stopifnot(is.character(.status))
  check_crucial_names(x, "status")
  is_possible_status <- unique(x$status)
  stopifnot(.status %in% is_possible_status)
}

