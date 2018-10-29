#' Import mapping each spreadsheet of an excel file to a dataframe in a list.
#' 
#' A useful complement of this function is [list_csv()].
#'
#' @param path A path to a single excel file.
#' 
#' @seealso [list_csv()].
#' @family functions to handle multiple spreadsheets of an excel workbook.
#' @family general functions to import data
#'
#' @source Adapted from an article by Jenny Bryan (https://goo.gl/ah8qkX).
#' @return A list of dataframes.
#'
#' @export
#' @examples
#' xlsheets_list(tool_example("multiple_sheets.xlsx"))
xlsheets_list <- function(path) {
 # Piping to avoid useless intermediate variables
 path %>%
   readxl::excel_sheets() %>%
   set_names() %>%
   purrr::map(readxl::read_excel, path = path)
}



# files_to_df <- function(.map, .read, ext) {
#   function(dir, ...) {
#     files <- fs::dir_ls(dir, regexp = ext)
#     lst <- .map(files, .read, ...)
#     set_names(lst, fs::path_file(names(lst)))
#   }
# }
# 
# 
# 
# #' Import reducing multiple .csv or flat .xlsx files into a single dataframe.
# #' 
# #' Read all .csv (`csv_*()`), or excel files (`xl_*()`) from a directory into a
# #' single dataframe.
# #' 
# #' @template io
# #' @family general functions to import data
# #' @keywords internal
# #' 
# #' @return A single dataframe.
# #'
# #' @examples
# #' xl_df(tool_example("multiple_workbooks"))
# #' 
# #' csv_df(tool_example("multiple_csv"))
# #' @name files_to_df
# NULL
# 
# #' @export
# #' @rdname files_to_df
# csv_df <- files_to_df(purrr::map_df, readr::read_csv, "csv$")
# 
# #' @export
# #' @rdname files_to_df
# xl_df <- files_to_df(purrr::map_df,  readxl::read_excel, "xls|xlsx")
# 
# 
# 
# #' Import mapping each .csv or flat .xlsx file to a dataframe in a list.
# #' 
# #' Read all .csv (`csv_*()`), or excel files (`xl_*()`) from a directory into a
# #' list of dataframes, where each element of the list is named as the source
# #' file.
# #' 
# #' @template io
# #' @family general functions to import data
# #' 
# #' @return A list of dataframes.
# #' @examples
# #' xl_list(tool_example("multiple_workbooks"))
# #' 
# #' # Pass an argument to `read_excel()` via `...`
# #' xl_list(tool_example("multiple_workbooks"), sheet = 2)
# #' 
# #' csv_list(tool_example("multiple_csv"))
# #' 
# #' # Pass an argument to `read_csv()` via `...`
# #' csv_list(tool_example("multiple_csv"), n_max = 2)
# #' @name files_to_list
# NULL
# 
# #' @export
# #' @rdname files_to_list
# csv_list <- files_to_df(purrr::map, readr::read_csv, "csv$")
# 
# #' @export
# #' @rdname files_to_list
# xl_list <- files_to_df(purrr::map, readxl::read_excel, "xls|xlsx")




