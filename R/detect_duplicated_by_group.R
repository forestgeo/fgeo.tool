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



# TODO: Replace by functions above

#' Factory of functions to detect duplicated values of a variable by groups.
#' 
#' @param var Bare name of a variable.
#' 
#' @family functions for developers. 
#' @family function factories.
#' @family predicates.
#'
#' @return A closure with the formal argument `.data` that returns `TRUE` or 
#'   when duplicated values of `var` are detected or `FALSE` if not.
#' @export
#' 
#' @examples
#' detect_duplicated_treeid_by_group <- detect_duplicated_var(treeid)
#' tree <- tibble::tibble(treeID = c(1, 1))
#' detect_duplicated_var(treeid)(tree)
detect_duplicated_var <- function(var) {
  function(.data) {
    var <- enquo(var)
    
    .x <- rlang::set_names(.data, tolower)
    .x <- groups_lower(.x)
    
    out <- .x %>% 
      dplyr::group_by(!! var, add = TRUE) %>% 
      dplyr::count()
    
    any(out$n > 1)
  }
}
