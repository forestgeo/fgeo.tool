#' Export mapping each dataframe in a list to a single .csv file.
#'
#' A useful complement of this function is [xlsheets_list()].
#'
#' @source Adapted from an article by Jenny Bryan (https://goo.gl/ah8qkX).
#'
#' @param dfs A list of dataframes.
#' @param dir Character; the directory where the files will be saved.
#' @param prefix Character; a prefix to add to the file names.
#'
#' @seealso xlsheets_list
#' @family functions to handle multiple spreadsheets of an excel workbook.
#' @family general functions to export data
#' 
#' @export
#' @examples
#' dfs <- xlsheets_list(tool_example("multiple_sheets.xlsx"))
#' 
#' # Saving the output to a temporary file
#' output <- tempdir()
#' list_csv(dfs, output, prefix = "myfile-")
#'
#' # Look inside the output directory to confirm it worked
#' dir(output, pattern = "myfile")
list_csv <- function(dfs, dir, prefix = NULL) {
  stopifnot(is.list(dfs), each_list_item_is_df(dfs), is.character(dir))
  if (!is.null(prefix)) {
    stopifnot(is.character(prefix))
  }
  validate_dir(dir = dir, dir_name = "`dir`")

  purrr::walk2(dfs, names(dfs), list_csv_, prefix = prefix, dir = dir)
}

validate_dir <- function(dir, dir_name) {
  invalid_dir <- !fs::dir_exists(dir)
  if (invalid_dir) {
    msg <- paste0(
      dir_name, " must match a valid directory.\n",
      "bad ", dir_name, ": ", "'", dir, "'"
    )
    abort(msg)
  } else {
    invisible(dir)
  }
}

#' Do list_csv() for each df.
#' @noRd
list_csv_ <- function(df, df_name,  prefix = NULL, dir) {
  path <- file.path(paste0(dir, "/", prefix, df_name, ".csv"))
  readr::write_csv(df, path)
}



#' Help export a single file by reducing a list of dataframes to a single one.
#'
#' This function wraps [purrr::reduce()] and [dplyr::full_join()] to reduce 
#' all or some dataframes in a list into a single dataframe.
#'
#' @param dfs A list of dataframes.
#' @param df_names Names of the list elements to join. `NULL` defaults to use
#'   all list elements.
#' @param by A character vector of variables to join by. If NULL, the default,
#'   *_join() will do a natural join, using all variables with common names
#'   across the two tables. To join by different variables on x and y use a
#'   named vector. For example, by = c("a" = "b") will match x.a to y.b. Passed
#'   to [dplyr::full_join()]
#'
#' @seealso [purrr::reduce()] and [dplyr::full_join()].
#' 
#' @family general functions to export data
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' dfs <- list(
#'   a = data.frame(x = 1),
#'   b = data.frame(x = 2, y = 2),
#'   c = data.frame(x = 1, z = 3)
#' )
#'
#' list_df(dfs, df_names = c("a", "c"))
#'
#' list_df(dfs, df_names = c("b", "c"))
#'
#' list_df(list(data.frame(1)))
#' # Use argument `by` if dataframes have no matching variable,
#' list_df(
#'   list(data.frame(x = 1), data.frame(z = 2)),
#'   by = c("x" = "z")
#' )
list_df <- function(dfs, df_names = NULL, by = NULL) {
  stopifnot(is.list(dfs), each_list_item_is_df(dfs))

  if (is.null(names(dfs))) {
    names(dfs) <- paste0("df", seq_along(dfs))
  }

  if (is.null(df_names)) {
    df_names <- names(dfs)
  } else {
    stopifnot(is.character(df_names))

    all_valid_nms <- all(purrr::map_lgl(df_names, ~rlang::has_name(dfs, .)))
    if (!all_valid_nms) {
      msg <- paste0(
        "Each value of `which` must be a valid name of `.df`\n",
        "* Values of `which` :", commas(df_names), "\n",
        "* Valid names of `.df` :", commas(names(dfs))
      )
      abort(msg)
    }
  }

  want <- purrr::keep(dfs, names(dfs) %in% df_names)
  suppressMessages(purrr::reduce(want, dplyr::full_join, by = by))
}
