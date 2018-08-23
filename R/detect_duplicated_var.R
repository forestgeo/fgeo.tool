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
flag_duplicated_by_group_f <- function(name) {
  force(name)
  function(.data, cond, msg = NULL) {
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


#' Factory to flag duplicated values of a variable (message, warning or error).
#' 
#' @param .f A condition function: One of message, warning, or stop.
#' @param var Bare name of a variable.
#' 
#' @family function factories.
#' @family functions for developers. 
#'
#' @return A closure with the formal argument `.data` that returns the flag
#'   produced by `.f` when duplicated values of `var` are detected.
#' @export
#' 
#' @examples
#' warn_duplicated_treeid <- flag_duplicated_var(warning, treeid)
#' tree <- tibble::tibble(treeID = c(1, 1))
#' warn_duplicated_treeid(tree)
#' flag_duplicated_var(message, treeid)(tree)
flag_duplicated_var <- function(.f, var) {
  function(.data) {
    var <- enquo(var)
    if (detect_duplicated_var(!! var)(.data)) {
      var <- rlang::quo_name(var)
      msg <- glue::glue("
        Detected duplicated values of {var}.
          Expected unique values of {var} within each data-group (if any)
      ")
      .f(msg)
    }
    invisible(.data)
  }
}

warn_duplicated_treeid <- flag_duplicated_var(rlang::warn, treeid)
