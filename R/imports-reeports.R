#' @importFrom glue glue glue_collapse
#' @importFrom purrr quietly
#' @importFrom rlang set_names is_named abort warn inform has_name %||%
#' @importFrom utils head tail
NULL
#' @export
rlang::`%||%`

globalVariables(c(".data", "."))

# dplyr -------------------------------------------------------------------

#' @importFrom dplyr filter mutate select arrange summarize summarise desc
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

# tidyselect --------------------------------------------------------------

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

# tibble ------------------------------------------------------------------

# Handle large datasets; print more info than dataframes and nicer
#  Document a returned tibble like so:
# @return a [tibble][tibble::tibble-package]
#' @importFrom tibble tibble tribble as_tibble
NULL
#' @export
tibble::tibble
#' @export
tibble::tribble
#' @export
tibble::as_tibble

# Other -------------------------------------------------------------------

# rlang: See utils-tidy-eval.R
# magrittr: See utils-pipe.R
