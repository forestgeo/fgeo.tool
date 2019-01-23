sanitize_view <- function(col_types) {
  function(x, na = c("", "NA", "NULL"), ...) {
    check_crucial_names(x, names(col_types))
    x <- purrr::modify(x, as.character)
    readr::type_convert(x, col_types = col_types, na = na, ...)
  }
}

#' Fix common problems in _ViewFullTable_ and _ViewTaxonomy_ data.
#'
#' These functions fix common problems of _ViewFullTable_ and _ViewTaxonomy_
#' data:
#' * Ensure that each column has the correct type.
#' * Ensure that missing values are represented with `NA`s -- not with the
#' literal string "NULL".
#'
#' @inheritParams readr::type_convert
#' @param x A dataframe; either a ForestGEO _ViewFullTable_ (`sanitize_vft()`)
#'   or _ViewTaxonomy_ (`sanitize_vft()`).
#' @param ... Arguments passed to [readr::type_convert()].
#'
#' @seealso [read_vft()].
#'
#' @section Acknowledgments:
#' Thanks to Shameema Jafferjee Esufali for motivating this functions.
#'
#' @return A dataframe.
#'
#' @examples
#' vft <- fgeo.x::vft_4quad
#' 
#' # Introduce problems to show how to fix them
#' # Bad column types
#' vft[] <- lapply(vft, as.character)
#' # Bad representation of missing values
#' vft$PlotName <- "NULL"
#' 
#' # "NULL" should be replaced by `NA` and `DBH` should be numeric
#' str(vft[c("PlotName", "DBH")])
#' 
#' # Fix
#' vft_sane <- sanitize_vft(vft)
#' str(vft_sane[c("PlotName", "DBH")])
#' 
#' taxa <- read.csv(fgeo.x::example_path("taxa.csv"))
#' # E.g. inserting bad column types
#' taxa[] <- lapply(taxa, as.character)
#' # E.g. inserting bad representation of missing values
#' taxa$SubspeciesID <- "NULL"
#' 
#' # "NULL" should be replaced by `NA` and `ViewID` should be integer
#' str(taxa[c("SubspeciesID", "ViewID")])
#' 
#' # Fix
#' taxa_sane <- sanitize_taxa(taxa)
#' str(taxa_sane[c("SubspeciesID", "ViewID")])
#' @family functions to edit ForestGEO data in place
#' @export
sanitize_vft <- sanitize_view(col_types = type_vft())

#' @rdname sanitize_vft
#' @export
sanitize_taxa <- sanitize_view(col_types = type_taxa())
