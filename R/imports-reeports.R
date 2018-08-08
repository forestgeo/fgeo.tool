#' @importFrom rlang set_names abort warn inform
#' @importFrom glue glue
#' @importFrom fgeo.base check_crucial_names rename_matches
#' @importFrom utils head tail
#' @importFrom measurements conv_unit
NULL

globalVariables(c(".data", "."))

# Tidy eval ---------------------------------------------------------------

#' Tidy eval helpers
#'
#' These functions provide tidy eval-compatible ways to capture
#' symbols (`sym()`, `syms()`, `ensym()`), expressions (`expr()`,
#' `exprs()`, `enexpr()`), and quosures (`quo()`, `quos()`, `enquo()`).
#' To learn more about tidy eval and how to use these tools, read
#' <http://rlang.tidyverse.org/articles/tidy-evaluation.html>
#'
#' @name tidyeval
#' @keywords internal
#' @aliases          quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @importFrom rlang quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @export           quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @importFrom rlang UQ UQS .data :=
NULL

# Reexport most commonly used funcitons from the tidyverse ----------------

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# Main verbs
#' @importFrom dplyr filter mutate select arrange summarize summarise
NULL
#' @export
dplyr::filter
#' @export
dplyr::mutate
#' @export
dplyr::select
#' @export
dplyr::arrange
#' @export
dplyr::summarize
#' @export
dplyr::summarise

# Allow using the main verbs by groups
#' @importFrom dplyr group_by ungroup
NULL
#' @export
dplyr::group_by
#' @export
dplyr::ungroup

# The single most useful and succing summary
#' @importFrom dplyr count add_count
NULL
#' @export
dplyr::count
#' @export
dplyr::add_count

# Unleash the power of dplyr::select(). See ?tidyselect::select_helpers
#' @importFrom tidyselect starts_with ends_with contains matches num_range
#' @importFrom tidyselect one_of everything last_col 
NULL
#' @export
tidyselect::starts_with
#' @export
tidyselect::ends_with
#' @export
tidyselect::contains
#' @export
tidyselect::matches
#' @export
tidyselect::num_range
#' @export
tidyselect::one_of
#' @export
tidyselect::everything
#' @export
tidyselect::last_col

# Handle large datasets; print more info than dataframes and nicer
#' @importFrom tibble tibble tribble as_tibble
NULL
#' @export
tibble::tibble
#' @export
tibble::tribble
#' @export
tibble::as_tibble
