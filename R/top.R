#' Filter a data set by matching n values from the head (or tail) of a variable.
#'
#' Filters a data frame by matching `n` values from the head (or tail) of the
#' unique values of a variable.
#'
#' Similar to [dplyr::top_n()], but instead of using `min_rank()` or
#' `max_rank()`, it uses [utils::head()] or [utils::tail()]; and `var` is
#' flexible as in [dplyr::pull()].
#'
#' @inheritParams dplyr::pull
#' @param n Number of values used for matching, from the head (or tail) of `var`.
#' @seealso [dplyr::pull], [dplyr::top_n], [utils::head()], [utils::tail()].
#'
#' @return A filtered version of the input dataset.
#' @export
#' @family functions to filter dataframes.
#'
#' @examples
#' df <- data.frame(x = 1:9, y = letters[1:3], stringsAsFactors = FALSE)
#'
#' # `var` can be bare or quoted
#' (result <- top(df, "y"))
#' identical(top(df, y), result)
#'
#' # matching `var` by position starting from the left
#' identical(top(df, var = y), top(df, var = 2))
#' # matching `var` by position starting from the right
#' identical(top(df, var = y), top(df, var = -1))
#'
#' top(df, y, n = 2)
#' # Negative values select from the tail
#' top(df, y, n = -2)
top <- function(.data, var, n = 1) {
  var <- rlang::enquo(var)
  pulled <- dplyr::pull(.data, !!var)
  sorted <- sort(unique(pulled))
  if (n > 0 ) {
    to_match <- head(sorted, n)
  } else {
    to_match <- tail(sorted, abs(n))
  }
  .data[pulled %in% to_match, ]
}
