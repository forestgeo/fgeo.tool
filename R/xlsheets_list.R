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

#' @rdname dir_list
#' @export
xl_list <- read_with(readxl::read_excel, regexp = "[.]xls$|[.]xlsx$")

#' @rdname dir_list
#' @export
xlbooks_list <- read_with(xlsheets_list, regexp = "[.]xls$|[.]xlsx$")
