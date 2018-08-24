# TODO Document

detect_duplicated_by_group_f <- function(name) {
  force(name)
  function(.data) {
    nested <- tidyr::nest(.data)$data
    any(purrr::map_lgl(nested, fgeo.base::detect_duplicated_f(name)))
  }
}

flag_predicate_by_group_f <- function(name, cond, predicate, prefix) {
  force(name)
  function(.data, msg = NULL) {
    stopifnot(length(cond) == 1)

    nested <- tidyr::nest(.data)$data
    detected <- any(purrr::map_lgl(nested, predicate(name)))
    if (detected) cond(msg %||% glue("{prefix} values were detected."))

    invisible(.data)
  }
}

flag_duplicated_by_group_f <- function(name, cond = warn) {
  flag_predicate_by_group_f(
    name, cond, fgeo.base::detect_duplicated_f, "Duplicated"
  )
}

flag_multiple_by_group_f <- function(name, cond = warn) {
  flag_predicate_by_group_f(
    name, cond, fgeo.base::detect_multiple_f, "Multiple"
  )
}
