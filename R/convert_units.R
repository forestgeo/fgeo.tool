#' Convert units of a vector or columns of a dataframe.
#' 
#' `convert_units()` of a vector or `convert_units_at()` specific columns of a
#' dataframe.
#'
#' @param x A vector or dataframe.
#' @param from,to String of the units to convert from/to as in [units::ud_units].
#' @inheritParams purrr::map_at
#' 
#' @seealso [units::valid_udunits()], [purrr::map_at()].
#'
#' @return An object with the same structure as `x` (vector or dataframe).
#' @export
#'
#' @examples
#' # View valid units with: View(units::valid_udunits())
#' convert_units(1:3, from = "mm", to = "m")
#' convert_units(1:3, "hectare", "m^2")
#' convert_units(1:3, "km/h", "m/h")
#' 
#' dfm <- data.frame(a = 1:2, b = 2:3)
#' convert_units_at(dfm, from = "m", to = "mm", .at = c("a", "b"))
#' convert_units_at(dfm, from = "m", to = "mm", .at = names(dfm))
#' convert_units_at(dfm, from = "m", to = "mm", .at = 2)
convert_units <- function(x, from, to) {
  units(x) <- units::as_units(from)
  units(x) <- units::as_units(to)
  units::drop_units(x)
}

#' @export
#' @rdname convert_units
convert_units_at <- function(x, .at, from, to) {
  x[] <- purrr::map_at(.x = x, .at = .at, ~convert_units(.x, from, to))
  x
}
 
