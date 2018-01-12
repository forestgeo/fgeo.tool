#' Suffix a strings where a vector exactly matches one specific string.
#'
#' @param string A vector of strings to suffix.
#' @param to_match A vector of strings to match with `.match`.
#' @param .match A string to match the vector `to_match` with.
#' @param suffix A suffix to add at the end of each element of `string`.
#'
#' @return A modified version of `x`.
#' @export
#' 
#' @examples
#' str_suffix_match(
#'   string = c("tag1", "tag2"),
#'   to_match = c("dead", "not-dead"),
#'   .match = "dead",
#'   suffix = ".d"
#' )
#' 
#' library(dplyr)
#' vft <- tribble(
#'   ~Tag, ~Status,
#'   "01", "dead",
#'   "02", "alive"
#' )
#' mutate(vft, tagged = str_suffix_match(Tag, Status, "dead", ".d"))
str_suffix_match <- function(string, to_match, .match, suffix) {
  check_str_suffix_match(
    string = string, to_match = to_match, .match = .match, suffix = suffix
  )
  
  if (!.match %in% to_match) {
    warning("No stem has status `", .match, "`. Is this what you expect?")
  }
  
  is_dead <- to_match == .match
  string[is_dead] <- paste0(string[is_dead], suffix)
  string
}

check_str_suffix_match <- function(string, to_match, .match, suffix) {
  not_all_inputs_are_characters <- !all(
    is.character(string),
    is.character(to_match),
    is.character(.match),
    is.character(suffix)
  )
  if (not_all_inputs_are_characters) {
    stop("Inputs must be characters", call. = FALSE)
  } else {
    invisible(string)
  }
}
