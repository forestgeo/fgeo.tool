#' Read all .csv or flat excel files from a directory into dataframes.
#' 
#' Read all .csv (`csv_*()`), or excel files (`xl_*()`) from a directory into a
#' single dataframe (`*_df()`) or a list of dataframes (`*_dfs()`) -- where each
#' element of the list is named as the source file. Excel files are treated as
#' if they were flat -- meaning that these `xl_*()` functions can read only a
#' single sheet (specified via the argument `sheet` passed to
#' [readxl::read_excel()] via `...`). For reading multiple all sheets in a single
#' file see [xlsheets_to_dfs()].
#' 
#' @param dir String giving the directory containing the excel workbooks
#'   to read from.
#' @param ... Arguments passed to [readr::read_csv()] (`csv_*()`) or
#'   [readxl::read_excel()] (`xl_*()`).
#' 
#' @seealso [readr::read_csv()], [readxl::read_excel()].
#' 
#' @family general functions to import/export data
#' 
#' @section Acknowledgment:
#' Thanks to Jessica Shue for inspiring this functions.
#'
#' @return The versions ending in `_df` output a single dataframe. The versions
#'   ending in `_dfs` output a list of dataframes.
#'
#' @examples
#' xl_to_dfs(tool_example("multiple_workbooks"))
#' 
#' xl_to_df(tool_example("multiple_workbooks"))
#' 
#' # Pass an argument to `read_excel()` via `...`
#' xl_to_dfs(tool_example("multiple_workbooks"), sheet = 2)
#' 
#' csv_to_dfs(tool_example("multiple_csv"))
#' 
#' csv_to_df(tool_example("multiple_csv"))
#' 
#' # Pass an argument to `read_csv()` via `...`
#' csv_to_dfs(tool_example("multiple_csv"), n_max = 2)
#' @name files_to_df
NULL

files_to_df <- function(.map, .read, ext) {
  function(dir, ...) {
    files <- fs::dir_ls(dir, regexp = ext)
    dfs <- .map(files, .read, ...)
    set_names(dfs, fs::path_file(names(dfs)))
  }
}

#' @export
#' @name files_to_df
csv_to_df <- files_to_df(purrr::map_df, readr::read_csv, "csv$")
#' @export
#' @name files_to_df
csv_to_dfs <- files_to_df(purrr::map, readr::read_csv, "csv$")
#' @export
#' @name files_to_df
xl_to_df <- files_to_df(purrr::map_df,  readxl::read_excel, "xls|xlsx")
#' @export
#' @name files_to_df
xl_to_dfs <- files_to_df(purrr::map, readxl::read_excel, "xls|xlsx")



#' Read one excel workbook and map each spreadsheet to a dataframe in a list.
#'
#' A useful complement of this function is [dfs_to_csv()].
#'
#' @param path A path to an excel file.
#'
#' @source Adapted from an article by Jenny Bryan (https://goo.gl/ah8qkX).
#' @return A list of dataframes.
#'
#' @seealso [dfs_to_csv()].
#' @family functions to handle multiple spreadsheets of an excel workbook.
#'
#' @export
#' @examples
#' xlsheets_to_dfs(tool_example("multiple_sheets.xlsx"))
xlsheets_to_dfs <- function(path) {
  # Piping to avoid useless intermediate variables
  path %>%
    readxl::excel_sheets() %>%
    set_names() %>%
    purrr::map(readxl::read_excel, path = path)
}
