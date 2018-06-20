#' Sanitize ViewFullTable and ViewTaxonomy.
#' 
#' These functions sanitize dataframes from ViewFullTable and ViewTaxonomy to 
#' achieve the same effect as reading with `read_vft()` and `read_taxa()`. The
#' most common problems that these functions fix are these:
#' * Ensure that each column has the correct type.
#' * Ensure that missing values are represented with `NA`s -- not with the 
#' literal string "NULL".
#' 
#' @param x A dataframe; either a ForestGEO ViewFullTable (`sanitize_vft()`) or
#'   ViewTaxonomy (`sanitize_vft()`).
#' @inheritParams readr::type_convert
#' 
#' @seealso [read_fgeo()].
#' 
#' @section Acknowledgments:
#' Thanks to Shameema Jafferjee Esufali for motivating this functions.
#'
#' @return A dataframe.
#'
#' @examples
#' vft <- fgeo.data::luquillo_vft_4quad
#' 
#' # Introdue problems to show how to fix them
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
#' 
#' 
#' taxa <- fgeo.data::luquillo_taxa
#' # Bad column types
#' taxa[] <- lapply(taxa, as.character)
#' # Bad representation of missing values
#' taxa$SubspeciesID <- "NULL"
#' 
#' # "NULL" should be replaced by `NA` and `ViewID` should be integer
#' str(taxa[c("SubspeciesID", "ViewID")])
#' 
#' # Fix
#' taxa_sane <- sanitize_taxa(taxa)
#' str(taxa_sane[c("SubspeciesID", "ViewID")])
#' @name sanitize
NULL

#' @rdname sanitize
#' @export
sanitize_vft <- function(x, 
                         col_types = type_vft(), 
                         na = c("", "NA", "NULL"), 
                         ...) {
  readr::type_convert(x, col_types = col_types, na = na, ...) 
}

#' @rdname sanitize
#' @export
sanitize_taxa <- function(x,
                          col_types = type_taxa(), 
                          na = c("", "NA", "NULL"), 
                          ...) {
  readr::type_convert(x, col_types = col_types, na = na, ...) 
}
