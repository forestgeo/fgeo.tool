#' Add columns `lx/ly`, `QX/QY`, `index`, `col/row`, `hectindex`, `quad`, `gx/gy`.
#' 
#' These functions add columns to position trees in a forest plot. They 
#' work with ViewFull-tables and also with census-tables (tree and stem). 
#' From the input table, most functions use only the `gx` and `gy` coordinates.
#' The exception is the function `add_gxgy()` which inputs quadrat information.
#' If your data lacks some important column, an error message will inform you
#' what the missing column is.
#'
#' These functions are adapted from the [CTFS R
#' Package](http://ctfs.si.edu/Public/CTFSRPackage/).
#'
#' @template x_fgeo
#' @inheritParams from_var_to_var
#' @param start Defaults to label the first quadrat as "0101". Use `0` to
#'   instead label it as "0000".
#' @param width Number; width to pad the labels of plot-columns and -rows.
#'
#' @return A modified version of the dataframe `x` with the additional
#'   variable(s) `var`.
#'
#' @examples
#' x <- tribble(
#'     ~gx,    ~gy,
#'       0,      0,
#'      50,     25,
#'   999.9, 499.95,
#'    1000,    500
#' )
#' 
#' # `gridsize` has a common default; `plotdim` is guessed from the data
#' add_lxly(x)
#' 
#' gridsize <- 20
#' plotdim <- c(1000, 500)
#' 
#' add_qxqy(x, gridsize, plotdim)
#' 
#' add_index(x, gridsize, plotdim)
#' 
#' add_hectindex(x, gridsize, plotdim)
#' 
#' add_quad(x, gridsize, plotdim)
#' 
#' add_quad(x, gridsize, plotdim, start = 0)
#' 
#' # `width` gives the nuber of digits to pad the label of plot-rows and
#' # plot-columns, e.g. 3 pads plot-rows with three zeros and plot-columns with
#' # an extra trhree zeros, resulting in a total of 6 zeros.
#' add_quad(x, gridsize, plotdim, start = 0, width = 3)
#' 
#' add_col_row(x, gridsize, plotdim)
#' 
#' 
#' # From `quadrat` or `QuadratName` --------------------------------------
#' x <- tribble(
#'   ~QuadratName,
#'         "0001",
#'         "0011",
#'         "0101",
#'         "1001"
#' )
#' 
#' # Output `gx` and `gy` ---------------
#' 
#' add_gxgy(x)
#' 
#' # Warning: The data may already have `gx` and `gx` columns
#' gxgy <- add_gxgy(fgeo.x::tree5)
#' select(gxgy, matches("gx|gy"))
#' 
#' # Output `col` and `row` -------------
#' 
#' # Create columns `col` and `row` from `QuadratName` with `tidyr::separate()`
#' # The argument `sep` lets you separate `QuadratName` at any positon
#' \dontrun{
#' tidyr_is_installed <- requireNamespace("tidyr", quietly = TRUE)
#' stringr_is_installed <- requireNamespace("stringr", quietly = TRUE)
#' 
#' if (tidyr_is_installed && stringr_is_installed) {
#'   library(tidyr)
#'   library(stringr)
#'   
#'   vft <- tibble(QuadratName = c("0001", "0011"))
#'   vft
#'   
#'   separate(
#'     vft,
#'     QuadratName, into = c("col", "row"),
#'     sep = 2
#'   )
#'   
#'   census <- select(fgeo.x::tree5, quadrat)
#'   census
#'   
#'   census$quadrat <- str_pad(census$quadrat, width = 4, pad = 0)
#'   
#'   separate(
#'     census,
#'     quadrat, into = c("col", "row"),
#'     sep = 2,
#'     remove = FALSE
#'   )
#' }
#' }
#' 
#' @family functions to add columns to dataframes
#' @family functions for ForestGEO data
#' @family functions for fgeo census
#' @family functions for fgeo vft
#' @name add_var
NULL

# Input gxgy --------------------------------------------------------------

add_var <- function(x, var, gridsize = 20, plotdim = NULL) {
  .x <- sanitize_xy(low(x))
  
  check_add_var(x = .x, var = var, gridsize = gridsize, plotdim = plotdim)
  
  if (is.null(plotdim)) {
    plotdim <- guess_plotdim(.x)
    message("* If guess is wrong, provide the correct argument `plotdim`")
  }

  result <- switch(
    var,
    lxly = {
      newcol <- gxgy_to_var(.x, var, gridsize, plotdim)
      .x <- tibble::add_column(.x, lx = newcol$lx, ly = newcol$ly)
       restore_add_var(.x, x)
    },
    qxqy = {
      newcol <- gxgy_to_var(.x, var, gridsize, plotdim)
      .x <- tibble::add_column(.x, QX = newcol$QX, QY = newcol$QY)
      restore_add_var(.x, x)
    },
    index = {
      newcol <- gxgy_to_var(.x, var, gridsize, plotdim)
      .x <- tibble::add_column(.x, index = newcol)
      restore_add_var(.x, x)
    },
    colrow = {
      newcol <- gxgy_to_var(.x, var = "rowcol", gridsize, plotdim)
      .x <- tibble::add_column(
        .x,
        col = pad_dbl(newcol$col, width = 2, pad = 0),
        row = pad_dbl(newcol$row, width = 2, pad = 0)
      )
      restore_add_var(.x, x)
    },
    hectindex = {
      newcol <- gxgy_to_var(.x, var, gridsize = NULL, plotdim)
      .x <- tibble::add_column(.x, hectindex = newcol)
      restore_add_var(.x, x)
    }
  )

  result
}

#' @rdname add_var
#' @export
add_lxly <- function(x, gridsize = 20, plotdim = NULL) {
  add_var(x, var = "lxly", gridsize = gridsize, plotdim = plotdim)
}

#' @rdname add_var
#' @export
add_qxqy <- function(x, gridsize = 20, plotdim = NULL) {
  add_var(x, var = "qxqy", gridsize = gridsize, plotdim = plotdim)
}

#' @rdname add_var
#' @export
add_index <- function(x, gridsize = 20, plotdim = NULL) {
  add_var(x, var = "index", gridsize = gridsize, plotdim = plotdim)
}

#' @rdname add_var
#' @export
add_col_row <- function(x, gridsize = 20, plotdim = NULL) {
  add_var(x, var = "colrow", gridsize = gridsize, plotdim = plotdim)
}

#' @rdname add_var
#' @export
add_hectindex <- function(x, gridsize = 20, plotdim = NULL) {
  add_var(x, var = "hectindex", plotdim = plotdim)
}

#' @rdname add_var
#' @export
add_quad <- function(x, 
                     gridsize = 20, 
                     plotdim = NULL, 
                     start = NULL, 
                     width = 2) {
  abort_bad_start(start)
  
  w_rowcol <- add_var(x, "colrow", gridsize = gridsize, plotdim = plotdim)
  if (identical(start, 0)) {
    w_rowcol$col <- as.numeric(w_rowcol$col) - 1
    w_rowcol$row <- as.numeric(w_rowcol$row) - 1
  }
  
  w_rowcol <- dplyr::mutate(
    w_rowcol,
    col = pad_dbl(col, width = width, pad = 0),
    row = pad_dbl(row, width = width, pad = 0),
    quad = paste_colrow(col, row),
    row = NULL,
    col = NULL
  )
  w_rowcol
}

paste_colrow <- function(col, row) {
  paste_each <- function(col, row) {
    if (is.na(col) || is.na(row)) return(NA)
    paste0(col, row)
  }
  purrr::map2_chr(col, row, paste_each)
}



abort_bad_start <- function(start) {
  if (!is.null(start) && !identical(start, 0)) {
    abort("`start` must be `NULL` or `0000`")
  }
}



#' Rename px/py to gx/gy if x lacks gx/gy but has px/py.
#' 
#' @param x fgeo dataframe.
#' @noRd
sanitize_xy <- function(x) {
  if (rename_pxpy(x)) {
    x <- nms_try_rename(x, "gx", "px")
    x <- nms_try_rename(x, "gy", "py")
  }
  x
}

gxgy_to_var <- function(.x, var, gridsize, plotdim) {
  .f <- utils::getFromNamespace(paste0("gxgy_to_", var), "fgeo.tool")
  if (identical(var, "hectindex")) {
    # `gridsize` is unused
    return(.f(.x$gx, .x$gy, plotdim = plotdim))
  }
  
  .f(.x$gx, .x$gy, gridsize = gridsize, plotdim = plotdim)
}

#' Restore column names.
#'
#' @param .x A dataframe; a modified version of `x` where px/py is renamed to
#'   gx/gy.
#' @param x A dataframe.
#' 
#' @noRd
restore_add_var <- function(.x, x) {
  .x <- restore_pxpy_if_necessary(.x, set_names(x, tolower))
  rename_matches(.x, x)
}

rename_pxpy <- function(x) {
  missing_names_gxgy <- !nms_has_any(x, "gx", "gy")
  has_names_pxpy <- nms_has_any(x, "px", "py")
  missing_names_gxgy && has_names_pxpy
}

restore_pxpy_if_necessary <- function(.x, x) {
  if (rename_pxpy(x)) {
    .x <- dplyr::rename(.x, px = .data$gx, py = .data$gy)
  }
  .x
}

check_add_var <- function(x, var, from, gridsize, plotdim) {
  stopifnot(is.data.frame(x))
  check_crucial_names(x, c("gx", "gy"))
  no_gx_is_na <- !any(is.na(x$gx))
  stopifnot(no_gx_is_na)
  no_gy_is_na <- !any(is.na(x$gy))
  stopifnot(no_gy_is_na)
  stopifnot(all(x$gx >= 0))
  stopifnot(all(x$gy >= 0))

  stopifnot(!missing(var))
  stopifnot(var %in% c("lxly", "qxqy", "colrow", "index", "hectindex"))

  stopifnot(is.numeric(gridsize))
  if (!is.null(plotdim)) stopifnot(is.numeric(plotdim))
  if (!is.null(plotdim)) stopifnot(length(plotdim) == 2)
  
  invisible(x)
}

# Input quadratname or quadrat --------------------------------------------

#' @rdname add_var
#' @export
add_gxgy <- function(x, gridsize = 20, start = 0) {
  gxgy <- quad_to_gxgy(x[[nm_quad(x)]], gridsize = gridsize, start = start)
  # cbind accepts duplicaed names. dplyr::bind_cols doesn't
  dplyr::bind_cols(x, gxgy)
}

nm_quad <- function(x) {
  grep("^quadrat$|^quadratname$", names(x), value = TRUE, ignore.case = TRUE)
}

quad_to_gxgy <- function(x, gridsize = 20, start = 0) {
  x = as.numeric(as.character(x))
  rowno = x %% 100 - start
  colno = floor(x / 100) - start
  data.frame(gx = colno * gridsize, gy = rowno * gridsize)
}
