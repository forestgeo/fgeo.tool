#' Import multiple files into a list using any given reader function.
#'
#' @param .f A function able to read the desired file format.
#' @param regexp A regular expression to match files of appropriate format.
#' 
#' @family general functions to import data
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
read_with <- function(.f, regexp = NULL) {
  function(path_dir, ...) {
    files <- fs::dir_ls(path_dir, regexp = regexp)
    file_names <- fs::path_ext_remove(fs::path_file(files))
    out <- purrr::map(files, .f, ...)
    rlang::set_names(out, file_names)
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
rdata_list <- read_with(function(.x) get(load(.x)), "rdata$|Rdata$")

#' @rdname dir_list
#' @export
rds_list <- read_with(readr::read_rds, "rds$")

#' @rdname dir_list
#' @export
csv_list <- read_with(readr::read_csv, "csv$")

#' @rdname dir_list
#' @export
tsv_list <- read_with(readr::read_tsv, "tsv$")

#' @rdname dir_list
#' @export
xl_list <- read_with(readxl::read_excel, "xls$|xlsx$")

#' @rdname dir_list
#' @export
xlbooks_list <- read_with(xlsheets_list, "xls$|xlsx$")
