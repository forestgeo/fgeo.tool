#' Modify a string to make it names-friendly (lowercase and without spaces).
#'
#' @param x A character string.
#'
#' @seealso `fgeo.tool::nms_tidy()`.
#' @family general functions to edit data in place
#'
#' @return A modified version of `x`.
#'
#' @export
#' @examples
#' messy <- "Hi yOu"
#'
#' to_tidy_names(messy)
#'
#' messy_named_string <- c(`Messy Name` = messy)
#' # Targets strings, not its names (Target names with `fgeo.tool::nms_tidy()`)
#' to_tidy_names(messy_named_string)
#'
#' dfm <- data.frame(1)
#' setNames(dfm, to_tidy_names(messy))
#'
#' # Makes more sense when operating on strings
#' setNames(list(1), to_tidy_names(messy))
to_tidy_names <- function(x) {
  stopifnot(is.character(x))
  gsub(" ", "_", tolower(x))
}
