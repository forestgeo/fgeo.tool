#' Import ViewFullTable and ViewTaxonomy from .tsv (or .csv) files.
#' 
#' These functions help you read text files containing ViewFullTable and
#' ViewTaxonomy tables. They avoid common problems related to column separators,
#' missing values, column names, and column types. Although you can customize
#' them, in most cases you should only need to provide the argument `file`.
#' 
#' @section Acknowledgments:
#' Thanks to Shameema Jafferjee Esufali for inspiring the feature that
#' automatically detects `delim` (issue 65).
#' 
#' @inheritParams readr::read_delim
#' @param delim Single character used to separate fields within a record. The
#'   default (`delim = NULL`) is to guess between comma or tab (`","` or `"\t"`).
#' @param ... Other arguments passed to [readr::read_delim()].
#' @seealso [readr::read_delim()], [type_vft()], [type_taxa()].
#' 
#' @family functions to read text files delivered by ForestgGEO's database.
#' @family functions to import/export ForestGEO data
#'
#' @return A dataframe (or tibble).
#'
#' @examples
#' \dontrun{
#' # Needs internet connection.
#' read_vft("http://bit.ly/fgeo-data-luquillo-vft-random")
#' read_taxa("http://bit.ly/fgeo-data-luquillo-taxa")
#' }
#' @name read_fgeo
NULL

read_fgeo <- function(col_types) {
  function(file, delim = NULL, na = c("", "NA", "NULL"), ...) {
    delim <- delim %||% guess_comma_or_tab(file, names(col_types))
    fgeo <- readr::read_delim(
      file = file, delim = delim, col_types = readr::cols(.default = "c"), 
      na = na, ...
    )
    readr::type_convert(fgeo, col_types = col_types)
  }
}

#' @rdname read_fgeo
#' @export
read_vft <- read_fgeo(col_types = type_vft())

#' @rdname read_fgeo
#' @export
read_taxa <- read_fgeo(col_types = type_taxa())



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
#' more safely by explicitly specifying what type to expect from each column of
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
#' @seealso [readr::read_delim()], [readr::read_csv()], [read_fgeo()].
#' 
#' @family functions to operate on column types.
#' @family functions to read text files delivered by ForestgGEO's database.
#' @family functions to import/export ForestGEO data
#' 
#' @keywords internal
#'
#' @return A list.
#' @export
#'
#' @examples
#' str(type_vft())
#' 
#' \dontrun{
#' # Run this with your own ViewFullTable
#' readr::read_csv("ViewFullTable.csv", col_types = type_vft())
#' }
#' 
#' str(type_taxa())
#' 
#' \dontrun{
#' # Run this with your own ViewTaxonomy
#' readr::read_delim("ViewTaxonomy.csv", col_types = type_taxa())
#' }
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

guess_comma_or_tab <- function(file, nms) {
  comma <- suppressWarnings(suppressMessages(readr::read_csv(file, n_max = 0)))
  if (identical(names(comma), nms)) return(",")
    
  "\t"
}
