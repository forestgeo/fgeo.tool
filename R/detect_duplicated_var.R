#' Factory of functions to detect duplicated values of a variable.
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
#' detect_duplicated_treeid <- detect_duplicated_var(treeid)
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

detect_duplicated_treeid <- detect_duplicated_var(treeid)

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
      msg <- "Detected duplicated treeid (each should be unique)."
      .f(msg)
    }
    invisible(.data)
  }
}

warn_duplicated_treeid <- flag_duplicated_var(warning, treeid)

