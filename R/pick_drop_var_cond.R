#' Pick and drop rows of a ForestGEO ViewFullTable or census table.
#'
#' These functions provide an expressive and convenient way to filter
#' ForestGEO's data. They let you remove missing values (with `na.rm = TRUE`)
#' but conservatively default to keeping NAs. This behavior is similar to base
#' subsetting and unlike `dplyr::filter()`; this conservative default matters
#' since the difference between missing trees and dead trees is important --
#' and you may want to include missing trees in your analysis.
#'
#' @param dfm Dataframe; A ForestGEO stem- or tree-table.
#' @param x An atomic vector; a single value against to compare each value of
#'   the variable encoded in the function name.
#' @param na.rm Set to `TRUE` if you want to remove missing values from the
#'   variable encoded in the function name.
#'
#' @seealso `dplyr::filter()`, `Extract` (`[`).
#'
#' @return Dataframe rows with matching conditions.
#'
#' @examples
#' cns <- data.frame(
#'   dbh = c(0, 50, 100, 150, NA, NA, NA),
#'   status = c(rep("A", 4), "M", "D", NA)
#'   # stringsAsFactors = FALSE
#' )
#' cns
#'
#' # <=
#' pick_dbh_max(cns, 100)
#' pick_dbh_max(cns, 100, na.rm = TRUE)
#' # >=
#' pick_dbh_min(cns, 100)
#' pick_dbh_min(cns, 100, na.rm = TRUE)
#' # <
#' pick_dbh_under(cns, 100)
#' pick_dbh_under(cns, 100, na.rm = TRUE)
#' # >
#' pick_dbh_over(cns, 100)
#' pick_dbh_over(cns, 100, na.rm = TRUE)
#' # Same, but `subset()` does not let you keep NAs.
#' subset(cns, dbh > 100)
#'
#' # ==
#' pick_status(cns, "A")
#' pick_status(cns, "A", na.rm = TRUE)
#' # !=
#' drop_status(cns, "D")
#' drop_status(cns, "D", na.rm = TRUE)
#'
#' # Compose
#' pick_dbh_over(drop_status(cns, "D", na.rm = TRUE), 100)
#' 
#' @family functions for fgeo census and vft
#' @family functions for fgeo census
#' @family functions for fgeo vft
#' @family functions to pick or drop rows of a ForestGEO dataframe
#' @name pick_drop
NULL

var_cond_x <- function(var, cond) {
  force(var)
  force(cond)
  function(dfm, x, na.rm = FALSE) {
    stopifnot(is.data.frame(dfm))
    stopifnot(!missing(x), is.logical(na.rm), length(x) == 1)

    old <- names(dfm)
    names(dfm) <- tolower(names(dfm))

    .var <- dfm[[tolower(var)]]
    rows <- do.call(cond, list(.var, x))
    if (na.rm) {
      dfm <- dfm[rows & !is.na(rows), , drop = FALSE]
    } else {
      dfm <- dfm[rows | is.na(rows), , drop = FALSE]
    }

    names(dfm) <- old
    dfm
  }
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
