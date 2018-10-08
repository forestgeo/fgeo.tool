#' Restructure data as a dataframe.
#'
#' The goal of this (generic) function (with methods for multiple fgeo classes)
#' is to produce a dataframe that helps you to work fluently with other general
#' purpose tools such as __dplyr__ and __ggplot2__.
#'
#' @param .x An fgeo object of supported class.
#' @param ... Other arguments passed to methods.
#'
#' @seealso [to_df.krig_lst()], [to_df.tt_lst()].
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

#' Restructure the output of `fgeo.habitat::krig()` as a dataframe.
#'
#' @param .x The output of [fgeo.habitat::krig()].
#' @param name Name for the column to hold soil variable-names.
#' @param item Character string; either "df" or "df.poly".
#' @inheritDotParams to_df
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' library(fgeo.habitat)
#' vars <- c("c", "p")
#' krig <- krig(soil_fake, vars, quiet = TRUE)
#' head(to_df(krig))
#' }
to_df.krig_lst <- function(.x, name = "var", item = "df", ...) {
  stopifnot(is.character(name), is.character(item))
  stopifnot(length(item) == 1, item == "df" || item == "df.poly")

  dfs <- lapply(.x, "[[", item)
  out <- Reduce(rbind, fgeo.base::name_dfs(dfs, name = name))
  out[c(name, setdiff(names(out), name))]
}



# Class tt_lst ------------------------------------------------------------

#' Restructure  the output of `tt_test()` as a dataframe.
#'
#' @param .x An object of class tt_lst.
#' @param ... Other arguments passed to [to_df()].
#'
#' @return A dataframe.
#'
#' @export
#' @examples
#' \dontrun{
#' library(fgeo.habitat)
#' cns <- luquillo_top3_sp
#' spp <- unique(cns$sp)
#' hab <- luquillo_habitat
#'
#' tt_lst <- tt_test(cns, spp, hab)
#' tt_df <- to_df(tt_lst)
#' head(tt_df)
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
  dplyr::select(x, 
    .data$habitat, .data$sp, .data$probability, .data$distribution,
    .data$stem_count, dplyr::everything()
  )
}

new_tt_df <- function(.x) {
  stopifnot(is.data.frame(.x))
  structure(.x, class = c("tt_df", class(.x)))
}



# demography_lst ----------------------------------------------------------

#' Transform the output of demography functions into a (tibble) dataframe.
#' 
#' Restructure  the output of `mortality()`, `recruitment()`, and `growth()` as
#' a dataframe:
#' * `to_df.demography_lst()`: Restructures results calculated across the entire
#'   census data.
#' * `to_df.demography_lst_by()`: Restructures results calculated `by` groups.
#'
#' @param .x An object of class demography_lst.
#' @param ... Other arguments passed to `to_df()`.
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


