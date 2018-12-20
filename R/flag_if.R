#' Flag if a vector or dataframe-column meets a condition.
#'
#' This function returns a condition (error, warning, or message) and its first
#' argument, invisibly. It is a generic. If the first input is a vector, it
#' evaluates it directly; if it is is a dataframe, it evaluates a given column.
#'
#' @param .data Vector.
#' @param name String. The name of a column of a dataframe.
#' @param predicate A predicate function.
#' @param condition A condition function (e.g. [stop()], [warning()],
#'   `rlang::inform()`).
#' @param msg String. An optional custom message.
#' @param ... Other arguments passed to methods.
#'
#' @return A condition (and `.data` invisibly).
#' 
#' @examples
#' # WITH VECTORS
#' dupl <- c(1, 1)
#' flag_if(dupl, is_duplicated)
#' # Silent
#' flag_if(dupl, is_multiple)
#'
#' mult <- c(1, 2)
#' flag_if(mult, is_multiple, message, "Custom")
#' # Silent
#' flag_if(mult, is_duplicated)
#'
#' # Both silent
#' flag_if(c(1, NA), is_multiple)
#' flag_if(c(1, NA), is_duplicated)
#'
#' # WITH DATAFRAMES
#' .df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)
#' flag_if(.df, "b", is_multiple)
#' flag_if(.df, "a", is_multiple)
#' flag_if(.df, "a", is_multiple, message, "Custom")
#' 
#' @family functions to throw conditions
#' @family functions for internal use in other fgeo packages
#' @keywords internal
#' @export
flag_if <- function(.data, ...) {
  UseMethod("flag_if")
}

#' @rdname flag_if
#' @export
flag_if.default <- function(.data,
                            predicate,
                            condition = warning,
                            msg = NULL,
                            ...) {
  stopifnot(length(condition) == 1)
  if (predicate(.data)) condition(msg %||% "Flagged values were detected.")
  invisible(.data)
}

#' @rdname flag_if
#' @export
flag_if.data.frame <- function(.data,
                               name,
                               predicate,
                               condition = warning,
                               msg = NULL,
                               ...) {
  name <- tolower(name)
  msg <- msg %||% paste0(name, ": Flagged values were detected.")
  flag_if(extract_column(.data, name), predicate, condition, msg)
  invisible(.data)
}

extract_column <- function(.data, name) {
  stopifnot(is.data.frame(.data))
  .data <- stats::setNames(.data, tolower(names(.data)))
  stopifnot_has_name(.data, name)
  .data[[name]]
}

stopifnot_has_name <- function(.data, name) {
  if (!utils::hasName(.data, name)) {
    stop(name, " is an invalid name", call. = FALSE)
  }
}
