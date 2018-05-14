#' `recode()` by looking up `old` and `new` values (from a lookup table).
#'
#' Use this function inside `dplyr::recode()` to recode a vector based on values
#' from two other vectors, where `old` and `new` codes are looked up. These
#' lookup vectors are commonly stored in a dataframe and come from a .csv or
#' spreadsheet file.
#'
#' @param old,new Vectors of equal length giving old and new codes.
#'
#' @seealso `dplyr::recode()`
#'
#' @return A "spliced" list with names from `old` and values from `new`. The
#' kind of data structure that you can feed to `...` in dplyr::recode()`.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' library(dplyr, warn.conflicts = FALSE)
#' library(rlang)
#'
#' look <- tibble(
#'   old = c("spp1", "unknown"),
#'   new = c("spp3", "spp4")
#' )
#'
#' lookup(look$old, look$new)
#'
#' x <- c("spp1", "spp2", "spp3", "unknown", "spp3", "unknown", "spp1", "spp1")
#' x
#' recode(x, lookup(look$old, look$new))
#' # Same
#' recode(x, !!!as.list(set_names(look$new, look$old)))
#'
#' dfm <- tibble(x = x)
#' mutate(dfm, new_x = recode(x, lookup(look$old, look$new)))
lookup <- function(old, new) {
  if (length(old) != length(new)) {
    stop("`old` and `new` must be of equal length.")
  }
  lookup <- rlang::set_names(new, old)
  rlang::splice(as.list(lookup))
}
