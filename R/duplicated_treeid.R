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
#' stem <- tibble(treeID = c(1, 1), stemID = c(1.1, 1.2))
#' warn_duplicated_treeid(stem)
#' 
#' # No duplicates
#' stem <- tibble(treeID = c(1, 2), stemID = c(1.1, 2.1))
#' warn_duplicated_treeid(tree)
#' 
#' vft <- tibble(TreeID = c(1, 2))
#' detect_duplicated_treeid(vft)
#' vft <- tibble(TreeID = c(1, 1))
#' detect_duplicated_treeid(vft)
#' 
#' # Warns even if the duplicatd treeid come from multiple censuses.
#' tree <- tibble(CensusID = c(1, 2), treeID = c(1, 1))
#' detect_duplicated_treeid(tree)
#' # Deal with this separately, e.g.
#' split_censusid <- split(tree, tree$CensusID)
#' any(purrr::map_lgl(split_censusid, detect_duplicated_treeid))
warn_duplicated_treeid <- function(.data) {
  if (detect_duplicated_treeid(.data)) {
    # if (any(n$n > 1)) {
    msg <- "Detected duplicated treeid (each should be unique)."
    warning(msg, call. = FALSE)
  }
  invisible(.data)
}

#' @rdname warn_duplicated_treeid
#' @export
detect_duplicated_treeid <- function(.data) {
  # Lowercase names and groups for work with both census and ViewFullTable
  .x <- rlang::set_names(.data, tolower)
  .x <- groups_lower(.x)
  
  out <- .x %>% 
    dplyr::group_by(.data$treeid, add = TRUE) %>% 
    dplyr::count()
  any(out$n > 1)
}

# TODO: REMOVE
# detect_duplicated_treeid <- function(.data) {
#   
#   if ("censusid" %in% names(.x)) {
#     .x <- dplyr::group_by(.x, .data$censusid)
#   }
#     .x <- dplyr::group_by(.x, .data$treeid, add = TRUE)
#     
#     n <- dplyr::summarize(.x, n = dplyr::n_distinct(.data$treeid))
#     any(n$n > 1)
# }
