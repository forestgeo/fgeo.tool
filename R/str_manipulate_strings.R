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
  if (!is.character(string)) {
    rlang::warn("`string` is not of class character")
  }
  not_all_inputs_are_characters <- !all(
    is.character(to_match),
    is.character(.match),
    is.character(suffix)
  )
  if (not_all_inputs_are_characters) {
    rlang::abort("Inputs must becharacters")
  } else {
    invisible(string)
  }
}



#' Create a names-friendly version of a string (lowercase and with no spaces).
#'
#' @param x A character string.
#'
#' @seealso [nms_tidy()].
#' @return A modified version of `x`.
#' 
#' @export
#' @examples
#' messy <- "Hi yOu"
#' 
#' str_as_tidy_names(messy)
#' 
#' messy_named_string <- c(`Messy Name` = messy)
#' # Targets strings -- not its names
#' str_as_tidy_names(messy_named_string)
#' # (To target names use `nms_tidy()` instead.)
#' nms_tidy(messy_named_string)
#' 
#' dfm <- data.frame(1)
#' setNames(dfm, str_as_tidy_names(messy))
#' 
#' # Makes more sense when operating on strings
#' setNames(list(1), str_as_tidy_names(messy))
str_as_tidy_names <- function(x) {
  stopifnot(is.character(x))
  gsub(" ", "_", tolower(x))
}
