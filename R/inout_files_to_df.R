files_to_df <- function(.map, .read, ext) {
  function(dir, ...) {
    files <- fs::dir_ls(dir, regexp = ext)
    dfs <- .map(files, .read, ...)
    set_names(dfs, fs::path_file(names(dfs)))
  }
}



#' Import reducing multiple .csv or flat .xlsx files into a single dataframe.
#' 
#' Read all .csv (`csv_*()`), or excel files (`xl_*()`) from a directory into a
#' single dataframe.
#' 
#' @template io
#' @family general functions to import data
#' 
#' @return A single dataframe.
#'
#' @examples
#' xl_to_df(tool_example("multiple_workbooks"))
#' 
#' csv_to_df(tool_example("multiple_csv"))
#' @name files_to_df
NULL

#' @export
#' @rdname files_to_df
csv_to_df <- files_to_df(purrr::map_df, readr::read_csv, "csv$")

#' @export
#' @rdname files_to_df
xl_to_df <- files_to_df(purrr::map_df,  readxl::read_excel, "xls|xlsx")



#' Import mapping each .csv or flat .xlsx file to a dataframe in a list.
#' 
#' Read all .csv (`csv_*()`), or excel files (`xl_*()`) from a directory into a
#' list of dataframes, where each element of the list is named as the source
#' file.
#' 
#' @template io
#' @family general functions to import data
#' 
#' @return A list of dataframes.
#' @examples
#' xl_to_dfs(tool_example("multiple_workbooks"))
#' 
#' # Pass an argument to `read_excel()` via `...`
#' xl_to_dfs(tool_example("multiple_workbooks"), sheet = 2)
#' 
#' csv_to_dfs(tool_example("multiple_csv"))
#' 
#' # Pass an argument to `read_csv()` via `...`
#' csv_to_dfs(tool_example("multiple_csv"), n_max = 2)
#' @name files_to_dfs
NULL

#' @export
#' @rdname files_to_dfs
csv_to_dfs <- files_to_df(purrr::map, readr::read_csv, "csv$")

#' @export
#' @rdname files_to_dfs
xl_to_dfs <- files_to_df(purrr::map, readxl::read_excel, "xls|xlsx")




#' Import mapping each spreadsheet of an excel file to a dataframe in a list.
#' 
#' A useful complement of this function is [dfs_to_csv()].
#'
#' @param path A path to a single excel file.
#' 
#' @seealso [dfs_to_csv()].
#' @family functions to handle multiple spreadsheets of an excel workbook.
#' @family general functions to import data
#'
#' @source Adapted from an article by Jenny Bryan (https://goo.gl/ah8qkX).
#' @return A list of dataframes.
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
