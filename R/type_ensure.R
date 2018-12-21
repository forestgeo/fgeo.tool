#' Ensure the specific columns of a dataframe have a particular type.
#' 
#' @param df A dataframe.
#' @param ensure_nms Character vector giving names of `df` to ensure `type`
#' @param type A string giving the type to ensure in columns `ensure_nms`
#' 
#' @seealso [purrr::modify_at()].
#'
#' @return A modified version of `df`, with columns (specified in `ensure_nms`)
#'   of type `type`.
#'
#' @examples
#' dfm <- tibble::tibble(
#'   w = c(NA, 1, 2),
#'   x = 1:3, 
#'   y = as.character(1:3), 
#'   z = letters[1:3]
#' )
#' dfm
#' type_ensure(dfm, c("w", "x", "y"), "numeric")
#' type_ensure(dfm, c("w", "x", "y", "z"), "character")
#' 
#' @family functions to operate on column types
#' @family functions for developers
#' @keywords internal
#' @export
type_ensure <- function(df, ensure_nms, type = "numeric") {
  ensure <- df[ensure_nms]
  is_type <- paste0("is.", type)
  is_to_fix <- !purrr::map_lgl(ensure, rlang::as_function(is_type))
  col_nms_to_fix <- names(ensure[is_to_fix])
  warn_if_changing_type(ensure = ensure, is_to_fix = is_to_fix, type = type)
  as_type <- paste0("as.", type)
  purrr::modify_at(df, col_nms_to_fix, rlang::as_function(as_type))
}

warn_if_changing_type <- function(ensure, is_to_fix, type) {
  nm_to_fix <- names(ensure[is_to_fix])
  if (any(is_to_fix)) {
    msg <- paste0(
      commas(nm_to_fix), " should be ", type, ". Type found: ",
      commas(unique(purrr::map_chr(ensure[nm_to_fix], typeof))), "\n",
      "* Changing type (of ", commas(nm_to_fix), ") accordingly."
    )
    warn(msg)
  }
}

