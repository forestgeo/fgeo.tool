# TODO Document

detect_duplicated_by_group_f <- function(name) {
  force(name)
  function(.data) {
    nested <- tidyr::nest(.data)$data
    any(purrr::map_lgl(nested, fgeo.base::detect_duplicated_f(name)))
  }
}

# TODO: Add flag_multiple_by_group_f then remove duplication
flag_predicate_by_group_f <- function(name, cond = rlang::warn, predicate, prefix) {
  force(name)
  function(.data, msg = NULL) {
    stopifnot(length(cond) == 1)

    nested <- tidyr::nest(.data)$data
    detected <- any(purrr::map_lgl(nested, predicate(name)))
    customized <- c(paste0(prefix, " values were detected.\n"), msg)
    if (detected) cond(msg %||% customized)

    invisible(.data)
  }
}


flag_duplicated_by_group_f <- function(name, cond = rlang::warn) {
  flag_predicate_by_group_f(name, cond, fgeo.base::detect_duplicated_f, "Duplicated")
}



# # TODO: Add flag_multiple_by_group_f then remove duplication
# flag_duplicated_by_group_f <- function(name, cond = rlang::warn) {
#   force(name)
#   function(.data, msg = NULL) {
#     stopifnot(length(cond) == 1)
# 
#     nested <- tidyr::nest(.data)$data
#     detected <- any(purrr::map_lgl(nested, fgeo.base::detect_duplicated_f(name)))
#     customized <- c("Duplicated values were detected.\n", msg)
#     if (detected) cond(msg %||% customized)
# 
#     invisible(.data)
#   }
# }

