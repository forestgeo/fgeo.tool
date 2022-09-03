#' Pick and drop rows from _ViewFullTable_, _tree_, and _stem_ tables.
#'
#' These functions provide an expressive and convenient way to pick specific
#' rows from ForestGEO datasets. They allow you to remove missing values (with
#' `na.rm = TRUE`) but conservatively default to preserving them. This behavior
#' is similar to [base::subset()] and unlike `dplyr::filter()`. This
#' conservative default is important because you want want to include missing
#' trees in your analysis.
#'
#' @template data_fgeo
#' @param value An atomic vector; a single value against to compare each value
#'   of the variable encoded in the function's name.
#' @param na.rm Set to `TRUE` if you want to remove missing values from the
#'   variable encoded in the function's name.
#'
#' @seealso `dplyr::filter()`, `Extract` (`[`).
#'
#' @return A dataframe similar to .`data` but including only the rows with
#'   matching conditions.
#'
#' @examples
#' # styler: off
#' census <- tribble(
#'   ~dbh, ~status,
#'      0,     "A",
#'     50,     "A",
#'    100,     "A",
#'    150,     "A",
#'     NA,     "M",
#'     NA,     "D",
#'     NA,      NA
#'   )
#' # styler: on
#'
#' # <=
#' pick_dbh_max(census, 100)
#' pick_dbh_max(census, 100, na.rm = TRUE)
#'
#' # >=
#' pick_dbh_min(census, 100)
#' pick_dbh_min(census, 100, na.rm = TRUE)
#'
#' # <
#' pick_dbh_under(census, 100)
#' pick_dbh_under(census, 100, na.rm = TRUE)
#'
#' # >
#' pick_dbh_over(census, 100)
#' pick_dbh_over(census, 100, na.rm = TRUE)
#' # Same, but `subset()` does not let you keep NAs.
#' subset(census, dbh > 100)
#'
#' # ==
#' pick_status(census, "A")
#' pick_status(census, "A", na.rm = TRUE)
#'
#' # !=
#' drop_status(census, "D")
#' drop_status(census, "D", na.rm = TRUE)
#'
#' # Compose
#' pick_dbh_over(
#'   drop_status(census, "D", na.rm = TRUE),
#'   100
#' )
#'
#' # More readable as a pipiline
#' census %>%
#'   drop_status("D", na.rm = TRUE) %>%
#'   pick_dbh_over(100)
#'
#' # Also works with ViewFullTables
#' # styler: off
#' vft <- tribble(
#'   ~DBH,   ~Status,
#'      0,   "alive",
#'     50,   "alive",
#'    100,   "alive",
#'    150,   "alive",
#'     NA, "missing",
#'     NA,    "dead",
#'     NA,        NA
#' )
#' # styler: on
#'
#' pick_dbh_max(vft, 100)
#'
#' pick_status(vft, "alive", na.rm = TRUE)
#'
#' @family functions for fgeo census and vft
#' @family functions for fgeo census
#' @family functions for fgeo vft
#' @family functions to pick or drop rows of a ForestGEO dataframe
#' @name pick_drop
NULL

var_cond_x <- function(variable, cond) {
  force(variable)
  force(cond)
  function(data, value, na.rm = FALSE) {
    stopifnot(is.data.frame(data))
    stopifnot(!missing(value), is.logical(na.rm), length(value) == 1)

    pick_rows <- function(data_, value, na.rm) {
      rows <- do.call(cond, list(data_[[tolower(variable)]], value))
      if (na.rm) {
        exclude_na(data_, rows)
      } else {
        include_na(data_, rows)
      }
    }

    result <- pick_rows(set_names(data, tolower), value, na.rm)
    set_names(result, names(data))
  }
}
exclude_na <- function(.data, rows) {
  .data[rows & !is.na(rows), , drop = FALSE]
}
include_na <- function(.data, rows) {
  .data[rows | is.na(rows), , drop = FALSE]
}

#' @rdname pick_drop
#' @export
pick_dbh_min <- var_cond_x("dbh", `>=`)

#' @rdname pick_drop
#' @export
pick_dbh_max <- var_cond_x("dbh", `<=`)

#' @rdname pick_drop
#' @export
pick_dbh_under <- var_cond_x("dbh", `<`)

#' @rdname pick_drop
#' @export
pick_dbh_over <- var_cond_x("dbh", `>`)

#' @rdname pick_drop
#' @export
pick_status <- var_cond_x("status", `==`)

#' @rdname pick_drop
#' @export
drop_status <- var_cond_x("status", `!=`)
