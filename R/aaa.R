# Suppress R CMD check note -----------------------------------------------

#' @importFrom dplyr mutate select arrange group_by ungroup %>%
#' @import rlang
#' @importFrom tibble tibble tribble
#' @importFrom utils head tail
NULL

#' @export
dplyr::`%>%`

globalVariables(c(".data"))
