#' Add columns `lx/ly`, `QX/QY`, `index`, `col/row`, `hectindex`, `quad`, `gx/gy`.
#'
#' These functions add columns to position trees in a forest plot. They work
#' with _ViewFullTable_, _tree_ and _stem_ tables. From the input table, most
#' functions use only the `gx` and `gy` columns (or equivalent columns). The
#' exception is the function `add_gxgy()` which inputs quadrat information. If
#' your data lacks some important column, an error message will inform you which
#' column is missing.
#'
#' These functions are adapted from the [CTFS R
#' Package](http://ctfs.si.edu/Public/CTFSRPackage/).
#'
#' @template data_fgeo
#' @inheritParams from_var_to_var
#' @param start Defaults to label the first quadrat as "0101". Use `0` to
#'   label it as "0000" instead.
#' @param width Number; width to pad the labels of plot-columns and -rows.
#'
#' @return For any given `var`, a function `add_var()` returns a modified
#'   version of the input dataframe, with the additional variable(s) `var`.
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
#' assert_is_installed("fgeo.x")
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
#'     QuadratName,
#'     into = c("col", "row"),
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
#'     quadrat,
#'     into = c("col", "row"),
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

add_var <- function(data, var, gridsize = 20, plotdim = NULL) {
  data_ <- sanitize_xy(low(data))

  check_add_var(data_, var = var, gridsize = gridsize, plotdim = plotdim)

  if (is.null(plotdim)) {
    plotdim <- guess_plotdim(data_)
    message("* If guess is wrong, provide the correct argument `plotdim`")
  }

  result <- switch(
    var,
    lxly = {
      newcol <- gxgy_to_var(data_, var, gridsize, plotdim)
      data_ <- tibble::add_column(data_, lx = newcol$lx, ly = newcol$ly)
      restore_add_var(data_, data)
    },
    qxqy = {
      newcol <- gxgy_to_var(data_, var, gridsize, plotdim)
      data_ <- tibble::add_column(data_, QX = newcol$QX, QY = newcol$QY)
      restore_add_var(data_, data)
    },
    index = {
      newcol <- gxgy_to_var(data_, var, gridsize, plotdim)
      data_ <- tibble::add_column(data_, index = newcol)
      restore_add_var(data_, data)
    },
    colrow = {
      newcol <- gxgy_to_var(data_, var = "rowcol", gridsize, plotdim)
      data_ <- tibble::add_column(
        data_,
        col = pad_dbl(newcol$col, width = 2, pad = 0),
        row = pad_dbl(newcol$row, width = 2, pad = 0)
      )
      restore_add_var(data_, data)
    },
    hectindex = {
      newcol <- gxgy_to_var(data_, var, gridsize = NULL, plotdim)
      data_ <- tibble::add_column(data_, hectindex = newcol)
      restore_add_var(data_, data)
    }
  )

  result
}

#' @rdname add_var
#' @export
add_lxly <- function(data, gridsize = 20, plotdim = NULL) {
  add_var(data, var = "lxly", gridsize = gridsize, plotdim = plotdim)
}

#' @rdname add_var
#' @export
add_qxqy <- function(data, gridsize = 20, plotdim = NULL) {
  add_var(data, var = "qxqy", gridsize = gridsize, plotdim = plotdim)
}

#' @rdname add_var
#' @export
add_index <- function(data, gridsize = 20, plotdim = NULL) {
  add_var(data, var = "index", gridsize = gridsize, plotdim = plotdim)
}

#' @rdname add_var
#' @export
add_col_row <- function(data, gridsize = 20, plotdim = NULL) {
  add_var(data, var = "colrow", gridsize = gridsize, plotdim = plotdim)
}

#' @rdname add_var
#' @export
add_hectindex <- function(data, gridsize = 20, plotdim = NULL) {
  add_var(data, var = "hectindex", plotdim = plotdim)
}

#' @rdname add_var
#' @export
add_quad <- function(data,
                     gridsize = 20,
                     plotdim = NULL,
                     start = NULL,
                     width = 2) {
  abort_bad_start(start)

  data_ <- add_var(data, "colrow", gridsize = gridsize, plotdim = plotdim)
  if (identical(start, 0)) {
    data_$col <- as.numeric(data_$col) - 1
    data_$row <- as.numeric(data_$row) - 1
  }

  data_ <- dplyr::mutate(
    data_,
    col = pad_dbl(col, width = width, pad = 0),
    row = pad_dbl(row, width = width, pad = 0),
    quad = paste_colrow(col, row),
    row = NULL,
    col = NULL
  )
  data_
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
sanitize_xy <- function(data) {
  if (rename_pxpy(data)) {
    data <- nms_try_rename(data, "gx", "px")
    data <- nms_try_rename(data, "gy", "py")
  }
  data
}

gxgy_to_var <- function(data, var, gridsize, plotdim) {
  .f <- utils::getFromNamespace(paste0("gxgy_to_", var), "fgeo.tool")
  if (identical(var, "hectindex")) {
    # `gridsize` is unused
    return(.f(data$gx, data$gy, plotdim = plotdim))
  }

  .f(data$gx, data$gy, gridsize = gridsize, plotdim = plotdim)
}

#' Restore column names.
#'
#' @param data_ A dataframe; a modified version of `x` where px/py is renamed to
#'   gx/gy.
#' @param x A dataframe.
#'
#' @noRd
restore_add_var <- function(data_, data) {
  data_ <- restore_pxpy_if_necessary(data_, set_names(data, tolower))
  rename_matches(data_, data)
}

rename_pxpy <- function(data) {
  missing_names_gxgy <- !nms_has_any(data, "gx", "gy")
  has_names_pxpy <- nms_has_any(data, "px", "py")
  missing_names_gxgy && has_names_pxpy
}

restore_pxpy_if_necessary <- function(data_, data) {
  if (rename_pxpy(data)) {
    data_ <- dplyr::rename(data_, px = .data$gx, py = .data$gy)
  }
  data_
}

check_add_var <- function(data, var, from, gridsize, plotdim) {
  stopifnot(is.data.frame(data))
  check_crucial_names(data, c("gx", "gy"))
  no_gx_is_na <- !any(is.na(data$gx))
  stopifnot(no_gx_is_na)
  no_gy_is_na <- !any(is.na(data$gy))
  stopifnot(no_gy_is_na)
  stopifnot(all(data$gx >= 0))
  stopifnot(all(data$gy >= 0))

  stopifnot(!missing(var))
  stopifnot(var %in% c("lxly", "qxqy", "colrow", "index", "hectindex"))

  stopifnot(is.numeric(gridsize))
  if (!is.null(plotdim)) stopifnot(is.numeric(plotdim))
  if (!is.null(plotdim)) stopifnot(length(plotdim) == 2)

  invisible(data)
}

# Input quadratname or quadrat --------------------------------------------

#' @rdname add_var
#' @export
add_gxgy <- function(data, gridsize = 20, start = 0) {
  assert_quad(data)
 
  gxgy <- quad_to_gxgy(data[[nm_quad(data)]], gridsize = gridsize, start = start)
  out <- cbind(data, gxgy)
  # Repair names like dplyr::bind_cols for dplyr < 1.0.0
  nms <- make.unique(names(out))
  nms <- sub("gx.1", "gx1", nms)
  nms <- sub("gy.1", "gy1", nms)
  
  rlang::set_names(out, nms)
}

assert_quad <- function(data) {
  missing_quad <- !any(c("quadrat", "quadratname") %in% names(low(data)))
  if (missing_quad) {
    abort("Ensure your data has colum `quadrat` or `quadratname`.")
  }
}

nm_quad <- function(data) {
  grep("^quadrat$|^quadratname$", names(data), value = TRUE, ignore.case = TRUE)
}

quad_to_gxgy <- function(data, gridsize = 20, start = 0) {
  data <- as.numeric(as.character(data))
  rowno <- data %% 100 - start
  colno <- floor(data / 100) - start
  data.frame(gx = colno * gridsize, gy = rowno * gridsize)
}
