#' Create field forms forms.
#' 
#' Create field forms. Data should be from a single plot and of a
#' single census. Pandoc must be installed (see section Installing Pandoc).
#' 
#' @section Installing Pandoc:
#' This function requires that you have pandoc installed. See installation
#' instructions at https://pandoc.org/installing.html. To install pandoc from R
#' see the function `install.pandoc()` of the __installr__ package
#' (https://goo.gl/cpaxtd).
#'
#' @param forms A list of dataframes -- most likely the output of 
#'   [fieldforms_prepare()].
#' @param filename String; path to the output file. Must include the
#'   extension ".docx". Passed to [rmarkdown::render()].
#' @param output_format String. Currently only "word_document" is supported.
#' @param header A string to use as the header of the fieldform of each quadrat.
#' @param output_dir String giving a directory to save the file in.
#'
#' @seealso [fieldforms_header()].
#'
#' @return A (MS Word) .docx file.
#' @export
#'
#' @examples
#' \dontrun{
#' library(fgeo.tool)
#' x <- top4quad
#' prep <- fieldforms_prepare(x)
#' lapply(prep, head)
#' 
#' # You may customize the prepared data before making the fielforms. For example:
#' # Making names uppercase
#' prep <- lapply(prep, function(x) setNames(x, toupper(names(x))))
#' # Renaming a column
#' prep <- lapply(prep, function(x) dplyr::rename(x, COMMENTS = COMM))
#' # Removing a column
#' prep <- lapply(prep, function(x) dplyr::select(x, -NOTE))
#' # Adding a column
#' prep <- lapply(prep, function(x) dplyr::mutate(x, NEW = " "))
#' 
#' # Now you can make the fieldforms
#' tmp <- tempdir()
#' fieldforms_output(prep, output_dir = tmp)
#' fieldforms_output(
#'   prep, "fieldforms2.docx", header = "Custom Header", output_dir = tmp
#' )
#' 
#' # Confirm
#' dir(tmp)[grepl("docx$", dir(tmp))]
#' 
#' # Cleaning temporary directory
#' unlink(tmp)
#' }
fieldforms_output <- function(forms, 
                       filename = "fieldforms.docx", 
                       output_format = "word_document",
                       header = fieldforms_header(),
                       output_dir = getwd()) {
  check_fieldforms_output(
    forms = forms, filename = filename, output_format = output_format, 
    header = header
  )
  # `forms` and `header` pass to forms_loop.Rmd
  forms_loop <- system.file("extdata", "forms_loop.Rmd", package = "fgeo.tool")
  rmarkdown::render(
    forms_loop, 
    output_format = output_format, 
    output_file = filename,
    output_dir = output_dir
  )
}

check_fieldforms_output <- function(forms, filename, output_format, header) {
  stopifnot(is.list(forms))
  stopifnot(is.data.frame(forms[[1]]))
  stopifnot(is.character(filename))
  missing_docx_extension <- !grepl("docx$", filename)
  if (output_format == "word_document" && missing_docx_extension) {
    warn("`filename` should have the extension `.docx`")
  }
  stopifnot(is.character(header))
}

#' Prepare data to create field forms.
#' 
#' This function creates a data structure suitable for `fieldforms_output()`. The data
#' should be from a single plot and of a single census.
#'
#' @param x A dataframe; particularly a ForestGEO ViewFullTable.
#' @inheritParams add_subquad
#'
#' @return A list of restructured dataframes.
#' @export
#'
#' @examples
#' prepared <- fieldforms_prepare(top1quad)
fieldforms_prepare <- function(x,
                               y_q = 20,
                               x_q = 20,
                               x_sq = 5,
                               y_sq = 5,
                               subquad_offset = NULL) {
  x <- purrr::set_names(x, tolower)
  crucial <- c(
    "qx", "qy", "quadratname", "tag","stemid","mnemonic","dbh","hom","status"
  )
  fgeo.tool::check_crucial_names(x, crucial)
  msg <- paste0(
    "Filter data to keep a single census.\n",
    "Census found: ", commas(unique(x$censusid))
  )
  fgeo.tool::check_unique(x, "censusid", cond = "stop", msg = msg)
  
  x <- x %>% 
    fgeo.tool::add_subquad(
      x_q = x_q, y_q = y_q, x_sq = x_sq, y_sq = y_sq, 
      subquad_offset = subquad_offset
    ) %>% 
    dplyr::select(
      .data$quadratname, .data$subquadrat, .data$tag, .data$stemid, 
      .data$mnemonic, .data$dbh, .data$hom, .data$status
    ) %>% 
    dplyr::rename(sq = .data$subquadrat) %>% 
    dplyr::mutate(
      quadratname = stringr::str_pad(.data$quadratname, 4, pad = 0)
    ) %>% 
    dplyr::arrange(
      .data$quadratname, .data$sq, .data$tag, .data$stemid, .data$mnemonic
    ) %>% 
    dplyr::mutate(
      .dbh = "", 
      code = "", 
      .hom = "", 
      note = "",
      comm = ""
    )
  split(x, x$quadratname) %>% 
    purrr::map(dplyr::select, -.data$quadratname)
}

#' Header for fieldforms.
#' 
#' @return A srting.
#' @export
#'
#' @examples
#' fieldforms_header()
fieldforms_header <- function() {
  paste0(
    "Measured by .................  Recorded by ................. ",
    "Date ................. Data entry by ................. "
  )
}
