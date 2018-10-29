#' @description 
#' Excel files are treated as if they were flat: This function can read only a
#' single sheet (specified via the argument `sheet` passed to
#' [readxl::read_excel()] via `...`). For reading multiple all sheets in a
#' single file see [xlsheets_list()].
#' 
#' @param dir String giving the directory containing the .csv or excel files to
#'   import.
#' @param ... Arguments passed to [readr::read_csv()] (`csv_*()`) or
#'   [readxl::read_excel()] (`xl_*()`).
#' 
#' @seealso [readr::read_csv()], [readxl::read_excel()].
#' 
#' 
#' @section Acknowledgment:
#' Thanks to Jessica Shue for inspiring this functions.

