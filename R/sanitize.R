#' Sanitize ViewFullTable and ViewTaxonomy.
#' 
#' These functions sanitize dataframes from ViewFullTable and ViewTaxonomy to 
#' achieve the same effect as reading with `read_vft()` and `read_taxa()`. The
#' most common problems that these functions fix are these:
#' * Ensure that each column has the correct type.
#' * Ensure that missing values are represented with `NA`s -- not with the 
#' literal string "NULL".
#' 
#' @param vft A dataframe; particularly a ForestGEO ViewFullTable (`vft`).
#' @param taxa A dataframe; particularly a ForestGEO ViewTaxonomy (`taxa`).
#' 
#' @seealso [read_fgeo()].
#' 
#' @section Acknowledgments:
#' Thanks to Shameema Jafferjee Esufali for motivating this functions.
#'
#' @return
#' @export
#'
#' @examples
#' # Example data
#' vft <- tibble::tibble(QuadratName = c(0000, 0001), DBH = c("NULL", "NULL"))
#' str(vft)
#' 
#' # Warns because lot's of columns are missing
#' sane <- sanitze_vft(vft)
#' str(sane, give.attr = FALSE)
#' 
#' taxa <- luquillo::ViewTaxonomy_luquillo
#' # Introduce mistakes
#' taxa$SubspeciesID <- "NULL"
#' taxa$SpeciesID <- as.double(taxa$SpeciesID)
#' str(taxa[1:3], give.attr = FALSE)
#' 
#' sane <- sanitze_taxa(taxa)
#' str(sane[1:3], give.attr = FALSE)
sanitze_vft <- function(vft) {
  tmp <- tempfile()
  readr::write_csv(vft, tmp)
  read_vft(tmp, delim = ", ")
}

sanitze_taxa <- function(taxa) {
  tmp <- tempfile()
  readr::write_csv(taxa, tmp)
  read_taxa(tmp, delim = ", ")
}
