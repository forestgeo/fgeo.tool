#' Import multiple files into a list using any given reading function.
#'
#' @param .f A function able to read the desired file format.
#' @inheritParams fs::dir_ls 
#' 
#' @family general functions to import data
#' @keywords internal
#'
#' @return A modified version of the input function, able to read all files
#'   from a directory (provided they all are of the suitable format).
#'  
#' @export
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
#' \dontrun{
#' if (!requireNamespace("rio"))
#'   stop("Please install rio with install.packges('rio') to run this example")
#' 
#' guess_list <- read_with(rio::import)
#' path_mixed_files <- tool_example("mixed_files")
#' dir(path_mixed_files)
#' guess_list(path_mixed_files)
#' }
read_with <- function(.f, regexp = NULL) {
  function(path_dir, ...) {
    files <- fs::dir_ls(path_dir, regexp = regexp, ignore.case = TRUE)
    file_names <- fs::path_ext_remove(fs::path_file(files))
    out <- lapply(files, .f, ...)
    stats::setNames(out, file_names)
  }
}

#' Import files (.csv, .tsv, excel sheets/workbooks, .Rdata, .rds) into a list.
#' 
#' @param path_dir String; the path to a directory containing the files to read
#'   (all must be of appropriate format; see examples).
#' @param ... Arguments passed to the reader function.
#'   
#' 
#' @family general functions to import data
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
#' path_xl <- tool_example("xl")
#' path_xl
#' dir(path_xl)
#' xl_list(path_xl)
#' 
#' path_books <- tool_example("multiple_workbooks")
#' dir(path_books)
#' xlbooks_list(path_books)
#' @name dir_list
NULL

#' @rdname dir_list
#' @export
rdata_list <- read_with(function(.x) get(load(.x)), regexp = "[.]rdata$")

#' @rdname dir_list
#' @export
rds_list <- read_with(readr::read_rds, regexp = "[.]rds$")

#' @rdname dir_list
#' @export
csv_list <- read_with(readr::read_csv, regexp = "[.]csv$")

#' @rdname dir_list
#' @export
tsv_list <- read_with(readr::read_tsv, regexp = "[.]tsv$")

#' @rdname dir_list
#' @export
xl_list <- read_with(readxl::read_excel, regexp = "[.]xls$|[.]xlsx$")

#' @rdname dir_list
#' @export
xlbooks_list <- read_with(xlsheets_list, regexp = "[.]xls$|[.]xlsx$")
