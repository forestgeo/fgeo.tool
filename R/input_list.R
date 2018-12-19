#' Import multiple files into a list using any given reading function.
#'
#' @param .f A function able to read the desired file format.
#' @inheritParams fs::dir_ls 
#'
#' @return A modified version of the input function, able to read all files
#'   from a directory (provided they all are of the suitable format).
#'  
#' @examples
#' path_rds <- tool_example("rds")
#' path_rds
#' dir(path_rds)
#' 
#' rds_list <- read_with(readr::read_rds)
#' rds_list(path_rds)
#' 
#' # Same
#' read_with(readr::read_rds)(path_rds)
#' 
#' # Read excel files --------------------------------------------------------
#' \dontrun{
#' if (!requireNamespace("readxl"))
#'   stop("Please install readxl with `install.packges('readxl')`")
#' path_xl <- tool_example("xl")
#' dir(path_xl)
#' read_with(readxl::read_excel)(path_xl)
#' }
#' 
#' # Read mixed files --------------------------------------------------------
#' \dontrun{
#' if (!requireNamespace("rio"))
#'   stop("Please install rio with `install.packges('rio')`")
#' if (!requireNamespace("readxl"))
#'   stop("Please install readxl with `install.packges('readxl')`")
#'
#' path_mixed_files <- tool_example("mixed_files")
#' dir(path_mixed_files)
#' read_with(rio::import)(path_mixed_files)
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
#' @param ... Arguments passed to the reader function.
#' 
#' @return A list of dataframes.
#' 
#' @examples 
#' path_rdata <- tool_example("rdata")
#' path_rdata
#' dir(path_rdata)
#' rdata_list(path_rdata)
#' 
#' path_rds <- tool_example("rds")
#' path_rds
#' dir(path_rds)
#' rds_list(path_rds)
#' 
#' path_csv <- tool_example("csv")
#' path_csv
#' dir(path_csv)
#' csv_list(path_csv)
#' 
#' path_tsv <- tool_example("tsv")
#' path_tsv
#' dir(path_tsv)
#' tsv_list(path_tsv)
#' 
#' # Weird: Tab separated columns in a file with .csv extension
#' path_weird <- tool_example("weird")
#' dir(path_weird)
#' # Extension is .csv, but this is not what you want
#' csv_list(path_weird)
#' # Use this instead
#' delim_list(path_weird, delim = "\t")
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

