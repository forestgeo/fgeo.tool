#' Create objects of class "data.frame" from other fgeo classes.
#' 
#' Most of the popular, general-purpose tools for data science input objects of
#' class "data.frame" (<https://www.tidyverse.org/>). However, several __fgeo__
#' functions (either inherited from the original CTFS R Package or contributed
#' by ForestGEO partners) output data of different class. Taking as input
#' different classes of __fgeo__ objects, `to_df()` provides a simple,
#' consistent way to create dataframes.
#' 
#' This generic provides methods for classes that cannot be correctly coerced
#' simply with [base::as.data.frame()] (or similar functions from the
#' __tibble__ package).
#'
#' @param .x An fgeo object of supported class.
#' @param ... Other arguments passed to methods.
#'
#' @seealso [to_df.krig_lst()],
#'   [to_df.tt_lst()],[fgeo.tool::to_df.demography_lst()],
#'   [fgeo.tool::to_df.demography_lst_by()].
#' 
#' @family fgeo generics
#' @keywords internal
#'
#' @return A dataframe.
#' @export
to_df <- function(.x, ...) {
  UseMethod("to_df")
}

#' @export
to_df.default <- function(.x, ...) {
  rlang::abort(glue("Can't deal with data of class {class(.x)}"))
}



# Class krig_lst ----------------------------------------------------------

#' Dataframe objects of class "krig_lst".
#' 
#' This method creates a dataframe from the output of `fgeo.habitat::krig()`
#' (which is a list of class "krig_lst").
#' 
#' @param .x The output of [fgeo.habitat::krig()].
#' @param name Name for the column to hold soil variable-names.
#' @param item Character string; either "df" or "df.poly".
#' @inheritDotParams to_df
#' 
#' @seealso [to_df()].
#' 
#' @family methods for fgeo generics
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' library(fgeo.habitat)
#' vars <- c("c", "p")
#' krig <- krig(soil_fake, vars, quiet = TRUE)
#' to_df(krig)
#' }
to_df.krig_lst <- function(.x, name = "var", item = "df", ...) {
  stopifnot(is.character(name), is.character(item))
  stopifnot(length(item) == 1, item == "df" || item == "df.poly")
  
  lst <- purrr::map(.x, item)
  purrr::map_dfr(lst, tibble::as.tibble, .id = name)
}

# Class tt_lst ------------------------------------------------------------

#' Dataframe objects of class "tt_lst".
#' 
#' This method creates a dataframe from the output of `fgeo.habitat::tt_test()`
#' (which is a list of class "tt_lst").
#' 
#' @param .x An object of class tt_lst.
#' @param ... Other arguments passed to [to_df()].
#' 
#' @seealso [to_df()].
#' 
#' @family methods for fgeo generics
#'
#' @return A dataframe.
#'
#' @export
#' 
#' @examples
#' if (requireNamespace("fgeo.habitat")) {
#'   library(fgeo.habitat)
#'   
#'   cns <- fgeo.habitat::luquillo_top3_sp
#'   hab <- fgeo.data::luquillo_habitat
#'   
#'   tt_lst <- tt_test(cns, unique(cns$sp), hab)
#'   to_df(tt_lst)
#' }
to_df.tt_lst <- function(.x, ...) {
  flip <- t(Reduce(rbind, .x))
  long_df <- tibble::as.tibble(
    fgeo.base::gather_mat(flip, "metric", "sp", "value")
  )
  with_habitat <- separate_habitat_metric(long_df)
  wide_df <- tidyr::spread(with_habitat, "metric", "value")
  wide_df <- dplyr::rename(
    wide_df, distribution = .data$Rep.Agg.Neut, stem_count = .data$N.Hab
  )
  
  explained <- explain_distribution(wide_df)
  
  out <- reorganize_columns(explained)
  out <- tibble::as.tibble(dplyr::arrange(out, .data$habitat, .data$sp))
  new_tt_df(out)
}

separate_habitat_metric <- function(x) {
  dplyr::mutate(x,
    habitat = stringr::str_replace(.data$metric, "^.*\\.([0-9]+$)", "\\1"),
    metric = stringr::str_replace(.data$metric, "(^.*)\\.[0-9]+$", "\\1")
  )
}

#' Form the authors of tt_test().
#' The Rep.Agg.Neut columns for each habitat indicate whether the sp is
#' significantly repelled (-1), aggregated (1), or neutraly distributed (0) on
#' the habitat in question.
#' @keywords internal
#' @noRd
explain_distribution <- function(x) {
  dplyr::mutate(x, 
    distribution = dplyr::case_when(
      .data$distribution == 1 ~ "aggregated",
      .data$distribution == -1 ~ "repelled",
      .data$distribution == 0 ~ "neutral",
      TRUE ~ NA_character_
    )
  )
}

reorganize_columns <- function(x) {
  first <- c("habitat", "sp", "distribution", "stem_count")
  x[c(first, setdiff(names(x), first))]
}

new_tt_df <- function(.x) {
  stopifnot(is.data.frame(.x))
  structure(.x, class = c("tt_df", class(.x)))
}



# Class demography_lst ----------------------------------------------------

#' Dataframe objects of class "demography_lst" and "demography_lst_by".
#' 
#' This method creates a dataframe from the output of
#' `fgeo.demography::mortality()`, `fgeo.demography::recruitment()`, and
#' `fgeo.demography::growth()` (each one is a list of class "demography_lst" if
#' `by` is `NULL`, or of class "demography_lst" if `by` is not `NULL`):
#' 
#' * `to_df.demography_lst()`: Restructures results calculated across the entire
#'   census data.
#' * `to_df.demography_lst_by()`: Restructures results calculated `by` groups.
#'
#' @param .x An object of class demography_lst.
#' @param ... Other arguments passed to `to_df()`.
#'
#' @seealso [to_df()].
#' 
#' @family methods for fgeo generics
#' 
#' @return A (tibble) dataframe.
#'
#' @export
#' @examples
#' \dontrun{
#' library(fgeo.data)
#' library(fgeo.demography)
#' 
#' censuses <- list(luquillo_tree5_random, luquillo_tree6_random)
#' to_df(mortality(censuses))
#' 
#' to_df(mortality(censuses, "sp"))
#' }
to_df.demography_lst <- function(.x, ...) {
  tidyr::unnest(tibble::enframe(.x, name = "metric"))
}

#' @rdname to_df.demography_lst
#' @export
to_df.demography_lst_by <- function(.x, ...) {
  out <- purrr::map_dfr(.x, ~tibble::enframe(.x, name = "by"), .id = "metric")
  out[c("by", "metric", "value")]
}

