# Excel -------------------------------------------------------------------

#' Combine spreadsheets from excel workbooks into .csv files (one per workbook).
#'
#' This is a rigid function with a very specific goal: To process
#' data from a specific sampling software. Specifically, this is what this
#' function does:
#' * Reads each spreadsheet from each workbook and map it to a dataframe.
#' * Lowercases and links the names of each dataframe.
#' * Keeps only these dataframes: (1) original_stems; (2) new_secondary_stems;
#'   (3) recruits; and (4) start_page.
#' * Keeps only non empty spreadsheets and warns what spreadsheets where dropped.
#' * Lowercases and links the names of each dataframe-variable.
#' * Drops fake stems.
#' * Joins all four dataframes into a single one.
#' * Output a single .csv file which name is prefixed with the name of the
#'   workbook.
#'
#' @param input_dir String giving the directory containing the excel workbooks
#'   to read from.
#' @param output_dir String giving the directory where to write .csv files to.
#'
#' @return Writes one .csv file for each workbook.
#' @export
#'
#' @examples
#' # Using sheets stored in the system.
#' path_to_example <- system.file("extdata", "example.xlsx", package = "qcr")
#' path_to_extdata <- fs::path(sub(basename(path_to_example), "", path_to_example))
#'
#' # This should be the path to the directory containing the excel files
#' sheets_directory <- path_to_extdata
#' # This should be the path to the directory to output the .csv files
#' output_directory <- tempdir()
#' # Do the work
#' xl_sheets_to_csv(sheets_directory, output_directory)
#'
#' # Confirm
#' fs::dir_ls(output_directory, regexp = "example.csv")
xl_sheets_to_csv <- function(input_dir, output_dir = "./") {
  check_xl_sheets_to_csv(
    input_dir = input_dir, indir_nm = "`input_dir`",
    output_dir = output_dir, outdir_nm = "`output_dir`"
  )
  purrr::walk2(xl_workbooks_to_chr(input_dir), output_dir, xl_sheets_to_csv_)
}

check_xl_sheets_to_csv <- function(input_dir,
                                   indir_nm,
                                   output_dir,
                                   outdir_nm) {
  stopifnot(is.character(input_dir), is.character(output_dir))
  purrr::map2(
    c(input_dir, output_dir), c("`input_dir`", "`output_dir`"), validate_dir
  )

  msg <- "`input_dir` must contain at least one excel file."
  file_names <- xl_workbooks_to_chr(input_dir)
  if (length(file_names) == 0) {
    abort(msg)
  }

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

#' Do xl_sheets_to_csv() for each excel file.
#' @noRd
xl_sheets_to_csv_ <- function(file,
                              output_dir = "./") {
  # Piping functions to avoid useless intermediate variables
  xl_list_sheets(file) %>%
    nms_tidy() %>%
    ensure_key_sheets() %>%
    purrr::keep(~!purrr::is_empty(.)) %>%
    lapply(nms_tidy) %>%
    drop_fake_stems() %>%
    name_listed_df(name = "sheet") %>%
    join_listed_df() %>%
    dplyr::mutate(unique_stem = paste0(.data$tag, "_", .data$stem_tag)) %>%
    readr::write_csv(reuse_filename(file = file, output_dir = output_dir))
}

ensure_key_sheets <- function(x) {
  key <- c("start_page", "orignal_stems", "new_secondary_stems", "recruits")
  missing_key_sheet <- !all(key %in% names(x))
  if (missing_key_sheet) {
    msg <- paste0(
      "Data should contain these sheets:\n", collapse(key), "\n",
      "* Sheets found: ", collapse(names(x))
    )
    abort(msg)
  }

  x[intersect(key, names(x))]
}

reuse_filename <- function(file, output_dir) {
  workbook_name <- sub("\\.xlsx$|\\.xls$", "", basename(file))
  file.path(paste0(output_dir, "/", workbook_name, ".csv"))
}

#' Remove rows equal to cero from the spreadsheet sheet new_secondary_stem.
#' @noRd
drop_fake_stems <- function(.df) {
  purrr::modify_at(.df, .at = "new_secondary_stems", ~.x[.x$new_stem != 0, ])
}
