# TODO: Move to fgeo.tool.

#' Detect and warn duplicated treeid.
#' 
#' @param .data A ForestGEO dataframe -- census or ViewFullTable.
#'
#' @return 
#' * `warn_duplicated_treeid()`: A warning or invisible `.data`.
#' * `detect_duplicated_treeid()`: `TRUE` or `FALSE`.
#' @export
#'
#' @examples
#' detect_duplicated_treeid(fgeo.data::luquillo_stem6_1ha)
#' warn_duplicated_treeid(fgeo.data::luquillo_stem6_1ha)
#' 
#' detect_duplicated_treeid(fgeo.data::luquillo_tree6_1ha)
#' warn_duplicated_treeid(fgeo.data::luquillo_tree6_1ha)
#' 
#' combo56 <- purrr::reduce(
#'   list(fgeo.data::luquillo_tree5_random, fgeo.data::luquillo_tree5_random),
#'   rbind
#' )
#' # Silent because it cheks for each censusid
#' detect_duplicated_treeid(combo56)
#' warn_duplicated_treeid(combo56)
warn_duplicated_treeid <- function(.data) {
  if (detect_duplicated_treeid(.data)) {
    # if (any(n$n > 1)) {
    msg <- "Detected duplicated `treeID` (all `treeID` should be unique)."
    warning(msg, call. = FALSE)
  }
  invisible(.data)
}

#' @rdname warn_duplicated_treeid
#' @export
detect_duplicated_treeid <- function(.data) {
  .x <- set_names(.data, tolower)
  
  if ("censusid" %in% names(.x)) {
    .x <- dplyr::group_by(.x, censusid)
  }
    .x <- dplyr::group_by(.x, treeid, add = TRUE)
    
    n <- dplyr::summarize(.x, n = dplyr::n_distinct(stemid))
    any(n$n > 1)
  }
