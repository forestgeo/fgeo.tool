# TODO Document

detect_duplicated_by_group_f <- function(name) {
  force(name)
  function(.data) {
    nested <- tidyr::nest(.data)$data
    any(purrr::map_lgl(nested, fgeo.base::detect_duplicated_f(name)))
  }
}

detect_duplicated_treeid_by_group <- detect_duplicated_by_group_f("treeid")


# TODO: Add flag_multiple_by_group_f then remove duplication
flag_duplicated_by_group_f <- function(name, cond = rlang::warn) {
  force(name)
  function(.data, msg = NULL) {
    stopifnot(length(cond) == 1)
    
    nested <- tidyr::nest(.data)$data
    customized <- c("Duplicated values were detected.\n", msg)
    has_dups <- any(purrr::map_lgl(nested, fgeo.base::detect_duplicated_f(name)))
    if (has_dups) cond(msg %||% customized)
    
    invisible(.data)
  }
}

