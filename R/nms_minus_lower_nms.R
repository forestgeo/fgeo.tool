#' Comparing two dataframes, How many names differ only in case?
#'
#' @param table1 A dataframe
#' @param table2 A dataframe
#'
#' @return An number indicating how many names are different only in their case.
#' @export
#'
#' @examples
#' \dontrun{
#' vft <- yosemite::ViewFullTable_yosemite
#' stem <- yosemite::yosemite_s1_lao
#' tree <- yosemite::yosemite_f1_lao
#' nms_minus_lower_nms(stem, vft)
#' nms_minus_lower_nms(vft, stem)
#' nms_minus_lower_nms(stem, tree)
#' }
nms_minus_lower_nms <- function(table1, table2) {
  stopifnot(is.data.frame(table1), is.data.frame(table2))

  nms_list <- purrr::map(list(table1, table2), names)
  diff_nms <- purrr::reduce(nms_list, setdiff)
  nms_list_lowered <- purrr::map(nms_list, tolower)
  diff_nms_lower <- purrr::reduce(nms_list_lowered, setdiff)
  length(diff_nms) - length(diff_nms_lower)
}
