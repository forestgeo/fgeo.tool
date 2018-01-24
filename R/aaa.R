# Suppress R CMD check note -----------------------------------------------

#' @importFrom dplyr mutate select arrange group_by ungroup %>%
#' @importFrom rlang .data enquo set_names
#' @importFrom tibble tibble tribble
#' @importFrom utils head tail
NULL

#' @export
dplyr::`%>%`

globalVariables(c(".data"))
