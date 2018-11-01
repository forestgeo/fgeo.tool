#' Import multiple .rdata files from a directory into a dataframe.
#' 
#' This is shortcut for `list_df(rdata_list(x))` with the additional ability to
#' read exclusively .rdata files containing a specific string. It is designed to
#' expect that all .rdata files contain dataframes with the same names -- as is
#' the case for multiple ForestGEO censuses delivered by the database. Notice
#' that function name have the format input_output, i.e. rdata_df.
#' 
#' @inheritParams dir_list
#' @param match If not `NULL` a sting to match specific .rdata file-names
#'   (excluding the extension).
#' @param .id If not NULL a variable with this name will be created to inform
#'   the source file.
#' 
#' @family general functions to import data
#' 
#' @return A dataframe.
#'
#' @export
#'
#' @examples
#' path_dir <- tool_example("rdata")
#' dir(path_dir)
#' 
#' rdata_df(path_dir)
#' 
#' rdata_df(path_dir, match = "tree6")
#' 
#' dfm <- rdata_df(path_dir, match = "tree5|6", .id = "source")
#' dfm
#' tail(dfm)
rdata_df <- function(path_dir, match = NULL, .id = NULL) {
  read_specific_rdata <- read_with(read_rdata, rdata_match(match))
  lst <- read_specific_rdata(path_dir)
  if (!all(purrr::map_lgl(lst, is.data.frame))) {
    abort("Can't read non-dataframe datasets.")
  }
  
  all_identical_names <- TRUE
  # TODO: Test when path_dir / is empty
  if (length(lst) > 1) {
    names_are_ok <- purrr::map_lgl(lst, ~identical(names(.x), names(lst[[1]])))
    all_identical_names <- all(names_are_ok)
  }
  
  if (!all_identical_names) {
    abort("Can't read datasets with different column names.")
  }
  
  purrr::map_dfr(lst, identity, .id = .id)
}

rdata_match <- function(match) {
  match <- match %||% ""
  glue(".*{match}.*[.]rdata$")
}

read_rdata <- function(.x) get(load(.x))
