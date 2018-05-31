#' @importFrom dplyr filter mutate select arrange group_by ungroup %>%
#' @import rlang
#' @importFrom tibble tibble tribble
#' @importFrom utils head tail
NULL

#' @export
dplyr::`%>%`

#' @importFrom fgeo.base name_df_lst
#' @export 
fgeo.base::name_df_lst

globalVariables(c(".data"))
