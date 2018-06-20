#' Convert units of a vector or columns of a dataframe.
#' 
#' `conv_unit()` of a vector or `conv_unit_at()` specific columns of a
#' dataframe.
#'
#' @param x A vector or dataframe.
#' @inheritParams measurements::conv_unit
#' @inheritParams purrr::map_at
#' 
#' @seealso [measurements::conv_unit()], [purrr::map_at()].
#'
#' @return An object with the same structure as `x` (vector or dataframe).
#' @export
#'
#' @examples
#' # For all units see ?measurements::conv_unit()
#' conv_unit(10, "mm2", "hectare")
#' conv_unit(1:3, from = "m", to = "mm")
#' conv_unit(1:3, "mph", "kph")
#' 
#' dfm <- data.frame(dbh = c(10, 100), name = c(10, 100))
#' conv_unit_at(dfm, .at = "dbh", from = "mm2", to = "hectare")
#' # All columns
#' conv_unit_at(dfm, .at = c("dbh", "name"), from = "mm2", to = "hectare")
#' # Same
#' conv_unit_at(dfm, .at = names(dfm), from = "mm2", to = "hectare")
conv_unit_at <- function(x, .at, from, to) {
  x[] <- purrr::map_at(
    .x = x, .at = .at, ~ measurements::conv_unit(.x, from, to)
  )
  x
}



#' Standardize specific columns of a dataframe.
#' 
#' @param x Dataframe.
#' @param total A numeric vector of length 1.
#' @inheritParams purrr::map_at
#'
#' @return A data.frame.
#' @export
#' 
#' @examples
#' dfm <- data.frame(a = 1:3, b = 11:13)
#' standardize_at(dfm, "a", total = 100)
#' standardize_at(dfm, names(dfm), total = 100)
standardize_at <- function(x, .at, total) {
  x[] <- purrr::map_at(x, .at = .at, ~ .x / total)
  x
}
