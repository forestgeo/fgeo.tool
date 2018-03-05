# types -------------------------------------------------------------------

#' Help to read ForestGEO data safely, with consistent column type.
#'
#' @description 
#' A common cause of problems is feeding functions with data which columns are
#' not all of the expected type. The problem often begins when reading data from
#' a text file with functions such as [utils::read.csv()],
#' [utils::read.delim()], and friends -- which commonly guess wrongly the column
#' type that you more likely expect. These common offenders are strongly
#' discouraged; instead consider using [readr::read_csv()], [readr::read_csv()],
#' and friends, which guess column types correctly much more often than their
#' analogs from the __utils__ package.
#' 
#' @description 
#' The functions `type_vft()`, `type_taxa()`, and friends help you to read data
#' more safely by explicitely specifying what type to expect from each column of
#' known datasets. These functions output the specification for the argument
#' `col_types()` of the functions `readr::read_*()`:
#' * `type_vft():` Type specification for ViewFullTable.
#' * `type_taxa():` Type specification for ViewFullTaxonomy.
#' 
#' @details
#' Types reference (for more details see [read_delim()]):
#' * c = character,
#' * i = integer,
#' * n = number,
#' * d = double,
#' * l = logical,
#' * D = date,
#' * T = date time,
#' * t = time,
#' * ? = guess,
#' * or _/- to skip the column.'.
#'
#' @seealso [readr::read_delim()], [readr::read_csv()].
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
