# types -------------------------------------------------------------------

# Types lookup (see ?readr::read_delim)
# c = character,
# i = integer,
# n = number,
# d = double,
# l = logical,
# D = date,
# T = date time,
# t = time,
# ? = guess,
# or _/- to skip the column.

#' Help read ForestGEO data with consistent column type.
#'
#' This functions helps to read ForestGEO's data with consistent column types. 
#' They output the specification for the argument `col_types()` of the functions
#' `read_*()` of the __readr__ package:
#' * `type_vft():` Type specification for ViewFullTable.
#' * `type_taxa():` Type specification for ViewFullTaxonomy.
#'
#' @seealso [readr::read_delim()].
#'
#' @return A list.
#' @export
#'
#' @examples
#' type_vft()
#' type_taxa()
#' @name type_fgeo

#' @rdname type_fgeo
#' @export
type_vft <- function() {
  list(
    DBHID = 'i',
    PlotName = 'c',
    PlotID = 'i',
    Family = 'c',
    Genus = 'c',
    SpeciesName = 'c',
    Mnemonic = 'c',
    Subspecies = 'c',
    SpeciesID = 'i',
    SubspeciesID = 'c',
    QuadratName = 'c',
    QuadratID = 'i',
    PX = 'd',
    PY = 'd',
    QX = 'd',
    QY = 'd',
    TreeID = 'i',
    Tag = 'c',
    StemID = 'i',
    StemNumber = 'i',
    StemTag = 'i',
    PrimaryStem = 'c',
    CensusID = 'i',
    PlotCensusNumber = 'i',
    DBH = 'd',
    HOM = 'd',
    ExactDate = 'D',
    Date = 'i',
    ListOfTSM = 'c',
    HighHOM = 'i',
    LargeStem = 'c',
    Status = 'c'
  )
}

#' @rdname type_fgeo
#' @export
type_taxa <- function() {
  list(
    ViewID = 'i',
    SpeciesID = 'i',
    SubspeciesID = 'c',
    Family = 'c',
    Mnemonic = 'c',
    Genus = 'c',
    SpeciesName = 'c',
    Rank = 'c',
    Subspecies = 'c',
    Authority = 'c',
    IDLevel = 'c',
    subspMnemonic = 'c',
    subspAuthority = 'c',
    FieldFamily = 'c',
    Lifeform = 'c',
    Description = 'c',
    wsg = 'd',
    wsglevel = 'c',
    ListOfOldNames = 'c',
    Specimens = 'c',
    Reference = 'c'
  )
}
