#' Combine spreadsheets from excel workbooks and output common data structures.
#' 
#' These functions combine spreadsheets from excel workbooks into common data
#' structures. `xl_sheets_to_csv()` and `xl_sheets_to_xl()` write a .csv or
#' excel (.xlsx) file per workbook -- combining all spreadsheets.
#' `xl_sheets_to_df` outputs a list where each dataframes combines all 
#' spreadsheeets of a workbook.
#'
#' This is a rigid function with a very specific goal: To process data from a
#' specific sampling software -- FastField. Specifically, this is what this
#' function does:
#' * Reads each spreadsheet from each workbook and map it to a dataframe.
#' * Lowercases and links the names of each dataframe.
#' * Keeps only these dataframes: (1) original_stems; (2) new_secondary_stems;
#'   and (3).
#' * Dates the data by `submission_id` (`date` comes from the spreadsheet
#' `root`).
#' * Lowercases and links the names of each dataframe-variable.
#' * Drops fake stems.
#' * Output a common data structure of your choice.
#' 
#'
#' @param input_dir String giving the directory containing the excel workbooks
#'   to read from.
#' @param output_dir String giving the directory where to write .csv files to.
#'
#' @return Writes one .csv file for each workbook.
#' 
#' @author Mauro Lepore and Jessica Shue.
#'
#' @examples
#' library(fs)
#' 
#' # Path to the folder I want to read excel files from
#' input_dir <- dirname(example_path("two_files/new_stem_1.xlsx"))
#' input_dir
#' 
#' # Files I want to read
#' dir(input_dir, pattern = "xlsx")
#' 
#' # Path to the folder I want to write .csv files to
#' output_dir <- tempdir()
#' 
#' # Do the work
#' xl_sheets_to_csv(input_dir, output_dir)
#' 
#' # Confirm
#' path_file(dir_ls(output_dir, regexp = "new_stem.*csv$"))
#' @name xl_sheets_to_output
NULL

xl_sheets_to_file <- function(ext, fun_write) {
    function(input_dir, output_dir = "./") {
    check_output_dir(output_dir = output_dir, print_as = "`output_dir`")
    dfs <- xl_sheets_to_df(input_dir = input_dir)
    files <- fs::path_ext_remove(names(dfs))
    paths <- fs::path(output_dir, fs::path_ext_set(files, ext))
    purrr::walk2(dfs, paths, fun_write)
  }
}

#' @export
#' @rdname xl_sheets_to_output
xl_sheets_to_csv <- xl_sheets_to_file("csv", readr::write_csv)

#' @export
#' @rdname xl_sheets_to_output
xl_sheets_to_xl <- xl_sheets_to_file("xlsx", writexl::write_xlsx)

#' @export
#' @rdname xl_sheets_to_output
xl_sheets_to_df <- function(input_dir) {
  check_input_dir(input_dir = input_dir, print_as = "`input_dir`")
  out <- purrr::map(xl_workbooks_to_chr(input_dir), xl_sheets_to_df_)
  purrr::set_names(out, basename(names(out)))
}

#' Do xl_sheets_to_df() for each excel file.
#' @noRd
xl_sheets_to_df_ <- function(file) {
  # Piping functions to avoid useless intermediate variables
  clean_dfm_list <- fgeo.tool::ls_list_spreadsheets(file) %>%
    fgeo.tool::nms_tidy() %>%
    ensure_key_sheets() %>%
    purrr::keep(~!purrr::is_empty(.)) %>%
    lapply(fgeo.tool::nms_tidy) %>%
    drop_fake_stems() %>%
    fgeo.tool::ls_name_df(name = "sheet") %>%
    warn_if_empty("new_secondary_stems") %>% 
    warn_if_empty("recruits") %>% 
    # Avoid merge errors
    coerce_as_character()
  
  with_date <- join_and_date(clean_dfm_list)
  # In columns matching "codes", replace commas by semicolon
  .df <- purrr::modify_if(
    with_date, grepl("codes", names(with_date)), ~gsub(",", ";", .x)
  )
  .df
}

#' Check that key spreadsheets exist.
#' @noRd
ensure_key_sheets <- function(x) {
  key <- c("original_stems", "new_secondary_stems", "recruits", "root")
  missing_key_sheet <- !all(key %in% names(x))
  if (missing_key_sheet) {
    msg <- paste0(
      "Data should contain these sheets:\n", commas(key), "\n",
      "* Missing sheets: ", commas(setdiff(key, names(x)))
    )
    abort(msg)
  }
  
  x[intersect(key, names(x))]
}

#' Remove rows equal to cero from the spreadsheet sheet new_secondary_stem.
#' @noRd
drop_fake_stems <- function(.df) {
  dropped <- purrr::modify_at(
    .df, .at = "new_secondary_stems", ~.x[.x$new_stem != 0, ]
  )
  dropped
}

#' Warns if a dataframe in a list of dataframes has empty rows.
#' @noRd
warn_if_empty <- function(.x, dfm_nm) {
  dfm <- .x[[dfm_nm]]
  has_cero_rows <- nrow(dfm) == 0
  if (has_cero_rows) {
    warn(paste0("`", dfm_nm, "`", " has cero rows."))
  }
  invisible(.x)
}

coerce_as_character <- function(.x, ...) {
  purrr::map(.x, ~purrr::modify(., .f = as.character, ...))
}

join_and_date <- function(.x) {
  # Join data from all sheets except from `root`
  is_not_root <- !grepl("root", names(.x))
  not_root_dfm <- .x %>% 
    purrr::keep(is_not_root) %>% 
    fgeo.tool::ls_join_df() %>% 
    dplyr::mutate(unique_stem = paste0(.data$tag, "_", .data$stem_tag))
  
  # From `root`, pull only `date` (plus a column to merge by)
  date <- .x[["root"]][c("submission_id", "date")]
  
  # Add date
  dplyr::left_join(not_root_dfm, date, by = "submission_id")
}

check_input_dir <- function(input_dir, print_as) {
  stopifnot(is.character(input_dir))
  validate_dir(input_dir, "`input_dir`")
  msg <- "`input_dir` must contain at least one excel file."
  file_names <- xl_workbooks_to_chr(input_dir)
  if (length(file_names) == 0) {
    abort(msg)
  }
  invisible()
}

check_output_dir <- function(output_dir, print_as) {
  stopifnot(is.character(output_dir))
  validate_dir(output_dir, "`output_dir`")
  invisible()
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

xl_workbooks_to_chr <- function(input_dir) {
  fs::dir_ls(input_dir, regexp = "\\.xls")
}
