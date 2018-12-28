#' Import multiple files into a list using any given reading function.
#' 
#' @inheritParams fs::dir_ls 
#' @param .f A function able to read the desired file format.
#'
#' @return A modified version of the input function, able to read all files
#'   from a directory (provided they all are of the suitable format).
#'  
#' @examples
#' rds_file <- tool_example("rds")
#' dir(rds_file)
#' 
#' rds_list <- read_with(readr::read_rds)
#' rds_list(rds_file)
#' 
#' # Same
#' read_with(readr::read_rds)(rds_file)
#' 
#' # Read specific files by matching file names via `regexp`
#' dir(rds_file)
#' read_with(readr::read_rds, regexp = "6")(rds_file)
#' read_with(readr::read_rds, regexp = "5[.]rds")(rds_file)
#' read_with(readr::read_rds, regexp = "tree.*[.]rds")(rds_file)
#' 
#' # Read excel files --------------------------------------------------------
#' excel_file <- tool_example("xl")
#' dir(excel_file)
#' \dontrun{
#' readxl_is_installed <- requireNamespace("readxl", quietly = TRUE)
#' if (readxl_is_installed) {
#'   library(readxl)
#'   
#'   read_with(readxl::read_excel)(excel_file)
#' }
#' }
#' 
#' # Read mixed files --------------------------------------------------------
#' mixed_files <- tool_example("mixed_files")
#' dir(mixed_files)
#' \dontrun{
#' readxl_is_installed <- requireNamespace("readxl", quietly = TRUE)
#' rio_is_installed <- requireNamespace("rio", quietly = TRUE)
#' if (rio_is_installed && readxl_is_installed) {
#'   library(rio)
#'   library(readxl)
#'   
#'   read_with(rio::import)(mixed_files)
#' }
#' }
#' @family general functions to import data
#' @export
read_with <- function(.f, regexp = NULL) {
  function(path_dir, ...) {
    files <- fs::dir_ls(path_dir, regexp = regexp, ignore.case = TRUE)
    if (length(files) == 0) {
      msg <- glue("
        Can't find any file with the desired extension:
          * Searching in: '{path_dir}'.
          * Searching for: '{regexp}'.
      ")
      abort(msg)
    }
    
    file_names <- fs::path_ext_remove(fs::path_file(files))
    out <- lapply(files, .f, ...)
    stats::setNames(out, file_names)
  }
}

#' Import multiple files (.csv, excel, .Rdata, ...) from a directory into a list.
#' 
#' These functions read from a specific directory where every file has its own extension
#' as indicated by each function's name. Notice that function names have the format
#' input_output, i.e. file-extension_list. If none of these functions do what
#' you want, create your own with [read_with()].
#' 
#' @param path_dir String; the path to a directory containing the files to read
#'   (all must be of appropriate format; see examples).
#' @param ... Arguments passed to the reader function:
#'   * `rdata_list()` and `rda_list()` read with `get(load(x))` (`...` not unused).
#'   * `rds_list()` reads with [readr::read_rds()].
#'   * `csv_list()` reads with [readr::read_csv()].
#'   * `delim_list()` reads with [readr::read_delim()].
#'   * `tsv_list()` reads with [readr::read_tsv()].
#' 
#' @return A list of dataframes.
#' 
#' @examples 
#' tool_example()
#' 
#' dir(tool_example("rdata"))
#' rdata_list(tool_example("rdata"))
#' 
#' dir(tool_example("rds"))
#' rds_list(tool_example("rds"))
#' 
#' dir(tool_example("csv"))
#' csv_list(tool_example("csv"))
#' 
#' dir(tool_example("tsv"))
#' tsv_list(tool_example("tsv"))
#' 
#' # Weird: Tab separated columns in a file with .csv extension
#' dir(tool_example("weird"))
#' 
#' # Extension is .csv, but this is not what you want
#' csv_list(tool_example("weird"))
#' 
#' # Use this instead
#' delim_list(tool_example("weird"), delim = "\t")
#' 
#' @family general functions to import data
#' @name dir_list
NULL

#' @rdname dir_list
#' @export
rdata_list <- read_with(function(.x) get(load(.x)), regexp = "[.]rdata$")

#' @rdname dir_list
#' @export
rda_list <- read_with(function(.x) get(load(.x)), regexp = "[.]rda$")

#' @rdname dir_list
#' @export
rds_list <- read_with(readr::read_rds, regexp = "[.]rds$")

#' @rdname dir_list
#' @export
csv_list <- read_with(readr::read_csv, regexp = "[.]csv$")

#' @rdname dir_list
#' @export
delim_list <- read_with(readr::read_delim, regexp = NULL)

#' @rdname dir_list
#' @export
tsv_list <- read_with(readr::read_tsv, regexp = "[.]tsv$")

