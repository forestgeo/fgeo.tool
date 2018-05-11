#' Read all .csv or excel files from a directory into dataframes.
#' 
#' Read all .csv, .xls, or .xlsx files from a directory into a dataframe or a 
#' list of dataframes, where each element of the list is named as the source
#' file.
#' 
#' @param input_dir String giving the directory containing the excel workbooks
#'   to read from.
#'
#' @return The versions ending in _df output a single dataframe. The versions
#'   ending in _lst output a list of dataframes.
#'
#' @examples
#' path <- system.file("extdata", "files/01.csv", package = "fgeo.tool")
#' input_dir <- fs::path_dir(path)
#' input_dir
#' dir(input_dir)
#' 
#' # Read all .csv files
#' csv_to_df(input_dir)
#' csv_to_df_lst(input_dir)
#' 
#' # Read all .xls or .xlsx files
#' xl_to_df_lst(input_dir)
#' xl_to_df(input_dir)
#' @name files_to_df
NULL

files_to_df <- function(.map, .read, ext) {
  function(input_dir) {
    files <- fs::dir_ls(input_dir, regexp = ext)
    dfs <- .map(files, .read)
    rlang::set_names(dfs, fs::path_file(names(dfs)))
  }
}

#' @export
#' @name files_to_df
csv_to_df <- files_to_df(purrr::map_df, readr::read_csv, "csv$")
#' @export
#' @name files_to_df
csv_to_df_lst <- files_to_df(purrr::map, readr::read_csv, "csv$")
#' @export
#' @name files_to_df
xl_to_df <- files_to_df(purrr::map_df,  readxl::read_excel, "xls|xlsx")
#' @export
#' @name files_to_df
xl_to_df_lst <- files_to_df(purrr::map, readxl::read_excel, "xls|xlsx")

