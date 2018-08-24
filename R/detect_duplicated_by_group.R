# TODO Document

detect_predicate_by_group_f <- function(name, predicate) {
  force(name)
  function(.data) {
    nested <- tidyr::nest(.data)$data
    any(purrr::map_lgl(nested, predicate(name)))
  }
}

#' @export
detect_duplicated_by_group_f <- function(name, predicate) {
  detect_predicate_by_group_f(name, fgeo.base::detect_duplicated_f)
}

#' @export
detect_multiple_by_group_f <- function(name, predicate) {
  detect_predicate_by_group_f(name, fgeo.base::detect_multiple_f)
}

flag_predicate_by_group_f <- function(name, cond, predicate, prefix) {
  force(name)
  function(.data, msg = NULL) {
    stopifnot(length(cond) == 1)

    nested <- tidyr::nest(.data)$data
    detected <- any(purrr::map_lgl(nested, predicate(name)))
    if (detected) cond(msg %||% glue("{name}: {prefix} values were detected."))

    invisible(.data)
  }
}

#' @export
flag_duplicated_by_group_f <- function(name, cond = warn) {
  flag_predicate_by_group_f(
    name, cond, fgeo.base::detect_duplicated_f, "Duplicated"
  )
}

#' @export
flag_multiple_by_group_f <- function(name, cond = warn) {
  flag_predicate_by_group_f(
    name, cond, fgeo.base::detect_multiple_f, "Multiple"
  )
}
