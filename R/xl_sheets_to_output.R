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
#' * Keeps only these dataframes: (1) original_stems, (2) new_secondary_stems,
#' and (3) "recruits".
#' * Dates the data by `submission_id` (`date` comes from the spreadsheet
#' `root`).
#' * Lowercases and links the names of each dataframe-variable.
#' * Drops fake stems.
#' * Output a common data structure of your choice.
#'
#' @param input_dir String giving the directory containing the excel workbooks
#'   to read from.
#' @param output_dir String giving the directory where to write .csv files to.
#' @param first_census This argument tells these functions what sheets to expect
#'   in the input. 
#'   * Use `TRUE` if this is your first census. The expected input must have
#'     sheets (1) "root", (2) "multi_stems", (3) "secondary_stems", and
#'     (4) "single_stems".
#'   * Use `FALSE` (default) if this is not your first census. The expected 
#'     input must have sheets (1) "root", (2) "original_stems", (3) 
#'     "new_secondary_stems", and (4) "recruits".
#'
#' @return Writes one .csv file for each workbook.
#' 
#' @author Mauro Lepore and Jessica Shue.
#' 
#' @section Acknowledgment:
#' * Sabrina Russo helped to make these functions useful with first censuses.
#' * David Orwig helped to fix a debug.
#'
#' @examples
#' library(fs)
#' library(readr)
#' library(readxl)
#' 
#' # NOT A FIRST CENSUS
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
#' # Output a csv file
#' xl_sheets_to_csv(input_dir, output_dir)
#' 
#' # Confirm
#' path_file(dir_ls(output_dir, regexp = "new_stem.*csv$"))
#' 
#' # Also possible to output excel and a list of dataframe. See next section.
#' 
#' # FIRST CENSUS
#' input_dir <- dirname(example_path("first_census/census.xlsx"))
#' # As a reminder you'll get a warning of missing sheets
#' # Output list of dataframes (one per input workbook -- here only one)
#' xl_sheets_to_df(input_dir, first_census = TRUE)
#' 
#' # Output excel
#' xl_sheets_to_xl(input_dir, output_dir, first_census = TRUE)
#' # Read back
#' filename <- path(output_dir, "census.xlsx")
#' out <- read_excel(filename)
#' str(out, give.attr = FALSE)
#' @name xl_sheets_to_output
NULL

xl_sheets_to_file <- function(ext, fun_write) {
    function(input_dir, output_dir = "./", first_census = FALSE) {
    check_output_dir(output_dir = output_dir, print_as = "`output_dir`")
    dfs <- xl_sheets_to_df(input_dir = input_dir, first_census = first_census)
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
xl_sheets_to_df <- function(input_dir, first_census = FALSE) {
  check_input_dir(input_dir = input_dir, print_as = "`input_dir`")
  out <- purrr::map(
    xl_workbooks_to_chr(input_dir), 
    xl_sheets_to_df_, first_census = first_census
  )
  purrr::set_names(out, basename(names(out)))
}

#' Do xl_sheets_to_df() for each excel file.
#' @noRd
xl_sheets_to_df_ <- function(file, first_census = FALSE) {
  dfm_list <- fgeo.tool::nms_tidy(fgeo.tool::ls_list_spreadsheets(file))
  
  if (first_census) {
    key <- key_first_census()
    dfm_list <- ensure_key_sheets(dfm_list, key)
  } else {
    key <- key_recensus()
    dfm_list <- ensure_key_sheets(dfm_list, key)
  }

  # Piping functions to avoid useless intermediate variables
  clean_dfm_list <- dfm_list %>% 
    purrr::keep(~!purrr::is_empty(.)) %>%
    lapply(fgeo.tool::nms_tidy) %>%
    drop_fake_stems()
  
  # After dropping fake stems new_secondary_stems might be empty (0-row)
  purrr::walk(names(clean_dfm_list), ~warn_if_empty(clean_dfm_list, .x))
  
  # Sanitize
  sane <- clean_dfm_list %>% 
    # Avoid error in naming cero-row dataframes
    warn_if_filling_cero_row_dataframe() %>% 
    purrr::modify_if(~nrow(.x) == 0, ~purrr::map_df(.x, ~NA)) %>% 
    fgeo.base::name_df_lst(name = "sheet") %>% 
    # Avoid merge errors
    coerce_as_character()
  
  with_date <- join_and_date(sane)
  # In columns matching "codes", replace commas by semicolon
  .df <- purrr::modify_if(
    with_date, grepl("codes", names(with_date)), ~gsub(",", ";", .x)
  )
  .df
}

#' Check that key spreadsheets exist.
#' @noRd
ensure_key_sheets <- function(x, key) {
  missing_key_sheet <- !all(names(key) %in% names(x))
  if (missing_key_sheet) {
    missing_sheets <- setdiff(names(key), names(x))
    msg <- paste0(
      "Adding missing sheets: ", commas(missing_sheets), "."
    )
    warn(msg)
    
    missing_appendix <- purrr::map(key[missing_sheets], str_df)
    x <- append(x, missing_appendix)
  }
  x
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
  
  if (is.null(dfm)) {
    warn(paste("`.x` has no dataframe", dfm_nm), ". Is this intentional?")
    return(invisible(.x))
  }
  
  has_cero_rows <- nrow(dfm) == 0
  if (has_cero_rows) {
    warn(paste0("`", dfm_nm, "`", " has cero rows."))
  }
  invisible(.x)
}

warn_if_filling_cero_row_dataframe <- function(dfs) {
  cero_row_dfs <- purrr::keep(dfs, ~nrow(.x) == 0)
  if (length(cero_row_dfs) != 0) {
    warning(
      "Filling every cero-row dataframe with NAs (", 
      commas(names(cero_row_dfs)), ").", 
      call. = FALSE
    )
  }
  invisible(dfs)
}

coerce_as_character <- function(.x, ...) {
  purrr::map(.x, ~purrr::modify(., .f = as.character, ...))
}

join_and_date <- function(.x) {
  # From `root`, pull only `date` (plus a column to merge by)
  date <- .x[["root"]][c("submission_id", "date")]
  
  # Join data from all sheets except from `root`
  is_not_root <- !grepl("root", names(.x))
  not_root_dfm <- purrr::keep(.x, is_not_root)
  
  # Collapse into a single dataframe, add variable, and join with date
  not_root_dfm %>% 
    fgeo.tool::ls_join_df() %>% 
    dplyr::mutate(unique_stem = paste0(.data$tag, "_", .data$stem_tag)) %>% 
    dplyr::left_join(date, by = "submission_id")
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
