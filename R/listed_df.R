#' Read an excel workbook and store each spreadsheet as a dataframe in a list.
#'
#' A useful complement of this function is [csv_listed_df()].
#'
#' @param path A path to an excel file.
#'
#' @source Adappted from an article by Jenny Bryan (https://goo.gl/ah8qkX).
#' @return A list of dataframes.
#'
#' @seealso csv_listed_df
#' @family functions to handle multiple spreadsheets of an excel workbook.
#'
#' @export
#' @examples
#' path_to_excel_workbook <- system.file(
#'   "extdata", "example.xlsx", package = "fgeo.tool"
#' )
#' str(xl_list_sheets(path_to_excel_workbook))
xl_list_sheets <- function(path) {
  # Piping to avoid useless intermediate variables
  path %>%
    readxl::excel_sheets() %>%
    rlang::set_names() %>%
    purrr::map(readxl::read_excel, path = path)
}



#' Mutate a list of dataframes to add column with the name of the list element.
#'
#' @param df_list A list of dataframes.
#' @param name A string giving the name of the column that stores the name of
#'   each dataframe of `x`.
#'
#' @return A modified version of `x`.
#' @export
#'
#' @examples
#' df_list <- list(a = data.frame(a = 1), data.frame(a = 2, b = 2))
#' df_list
#' name_listed_df(df_list)
#' name_listed_df(df_list, name = "sheet")
name_listed_df <- function(df_list, name = "name") {
  # Must capture argument `name` before it is evaluated
  .name <- rlang::quo_name(rlang::enquo(name))
  stopifnot(is.list(df_list), is.character(name))

  df_list <- fill_missing_names(df_list)
  nms <- names(df_list)
  for (i in seq_along(nms)) {
    df_list[[i]] <- dplyr::mutate(df_list[[i]], !! .name := nms[[i]])
  }
  df_list
}

fill_missing_names <- function(x) {
  filler_names <- paste0("df", seq_along(x))
  if (is.null(names(x))) {
    names(x) <- filler_names
  }
  missing_names <- names(x) %in% ""
  if (any(missing_names)) {
    names(x)[missing_names] <- filler_names[missing_names]
  }
  x
}



#' Save each dataframe in a list to a different .csv file.
#'
#' A useful complement of this function is [xl_list_sheets()].
#'
#' @source Adappted from an article by Jenny Bryan (https://goo.gl/ah8qkX).
#'
#'
#' @param df_list A list of dataframes.
#' @param dir Character; the directory where the files will be saved.
#' @param prefix Character; a prefix to add to the file names.
#'
#' @seealso xl_list_sheets.
#' @family functions to handle multiple spreadsheets of an excel workbook.
#' @export
#' @examples
#' path_to_excel_workbook <- system.file(
#'   "extdata", "example.xlsx", package = "fgeo.tool"
#' )
#'
#' df_list <- xl_list_sheets(path_to_excel_workbook)
#' str(df_list)
#'
#' output <- tempdir()
#' csv_listed_df(df_list, output, prefix = "myfile-")
#'
#' files <- dir(output)
#' files[grepl(".csv$", files)]
csv_listed_df <- function(df_list, dir, prefix = NULL) {
  stopifnot(is.list(df_list), each_list_item_is_df(df_list), is.character(dir))
  if (!is.null(prefix)) {
    stopifnot(is.character(prefix))
  }
  validate_dir(dir = dir, dir_name = "`dir`")

  purrr::walk2(
    df_list, names(df_list),
    csv_listed_df_, prefix = prefix, dir = dir
  )
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


#' Do csv_listed_df() for each df.
#' @noRd
csv_listed_df_ <- function(df, df_name,  prefix = NULL, dir) {
  path <- file.path(paste0(dir, "/", prefix, df_name, ".csv"))
  readr::write_csv(df, path)
}




#' Full-join all or some dataframes from a list of dataframes
#'
#' This function wraps [purrr::reduce()] and [dplyr::full_join()].
#'
#' @param df_list A list of dataframes.
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
#' @return A dataframe.
#' @export
#'
#' @examples
#' df_list <- list(
#'   a = data.frame(x = 1),
#'   b = data.frame(x = 2, y = 2),
#'   c = data.frame(x = 1, z = 3)
#' )
#'
#' join_listed_df(df_list, df_names = c("a", "c"))
#'
#' join_listed_df(df_list, df_names = c("b", "c"))
#'
#' join_listed_df(list(data.frame(1)))
#' # Use argument `by` if dataframes have no matching variable,
#' join_listed_df(
#'   list(data.frame(x = 1), data.frame(z = 2)),
#'   by = c("x" = "z")
#' )
join_listed_df <- function(df_list, df_names = NULL, by = NULL) {
  stopifnot(is.list(df_list), each_list_item_is_df(df_list))

  if (is.null(names(df_list))) {
    names(df_list) <- paste0("df", seq_along(df_list))
  }

  if (is.null(df_names)) {
    df_names <- names(df_list)
  } else {
    stopifnot(is.character(df_names))

    all_valid_nms <- all(purrr::map_lgl(df_names, ~rlang::has_name(df_list, .)))
    if (!all_valid_nms) {
      msg <- paste0(
        "Each value of `which` must be a valid name of `.df`\n",
        "* Values of `which` :", collapse(df_names), "\n",
        "* Valid names of `.df` :", collapse(names(df_list))
      )
      rlang::abort(msg)
    }
  }

  want <- purrr::keep(df_list, names(df_list) %in% df_names)
  suppressMessages(purrr::reduce(want, dplyr::full_join, by = by))
}
