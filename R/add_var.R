#' Add columns `lx/ly`; `QX/QY`; `index`; `col/row`; and `hectindex`, `quad`.
#' 
#' These functions add columns to position trees/stems in a forest plot. They 
#' work with ViewFull-tables and census-tables (tree and stem).
#'
#' These functions mostly wrap legacy code from the [CTFS R
#' Package](http://ctfs.si.edu/Public/CTFSRPackage/).
#'
#' @template x_fgeo
#' @inheritParams from_var_to_var
#' @param start `1` or `0`, indicating how to label the first plot-column.
#' @param width Number; width to pad the labels of plot-columns and -rows.
#'
#' @return A modified version of the dataframe `x` with the additional
#'   variable(s) `var`.
#'
#' @examples
#' x <- tibble::tibble(gx = c(0, 50, 999.9, 1000), gy = gx/2)
#' add_lxly(x)
#' add_qxqy(x)
#' add_index(x)
#' add_hectindex(x)
#' add_quad(x)
#' add_quad(x, start = 0)
#'
#' # `width` gives the nuber of digits to pad the label of plot-rows and
#' # plot-columns, e.g. 3 pads plot-rows with three zeros and plot-columns with
#' # an extra trhree zeros, resulting in a total of 6 zeros.
#' add_quad(x, start = 0, width = 3)
#' add_col_row(x)
#' 
#' # Column and row from QuadratName
#' x <- tibble::tribble(
#'   ~QuadratName,
#'   "0001",
#'   "0011",
#'   "0101",
#'   "1001"
#' )
#' add_col_row2(x)
#'
#' @family functions to add columns to dataframes
#' @family functions for ForestGEO data
#' @family functions for fgeo census
#' @family functions for fgeo vft
#' @name add_var
NULL

add_var <- function(x, var, gridsize = 20, plotdim = NULL) {
  .x <- set_names(x, tolower)
  .x <- sanitize_xy(.x)
  
  check_add_var(x = .x, var = var, gridsize = gridsize, plotdim = plotdim)

  if (is.null(plotdim)) {
    plotdim <- plotdim
    plotdim <- guess_plotdim(.x)
    message("* If guess is wrong, provide the correct argument `plotdim`")
  }

  if (var == "lxly") {
    newcol <- gxgy_to_var(.x, var, gridsize, plotdim)
    .x <- tibble::add_column(.x, lx = newcol$lx, ly = newcol$ly)
    return(restore_add_var(.x, x))
  }

  if (var == "qxqy") {
    newcol <- gxgy_to_var(.x, var, gridsize, plotdim)
    .x <- tibble::add_column(.x, QX = newcol$QX, QY = newcol$QY)
    return(restore_add_var(.x, x))
  }

  if (var == "index") {
    newcol <- gxgy_to_var(.x, var, gridsize, plotdim)
    .x <- tibble::add_column(.x, index = newcol)
    return(restore_add_var(.x, x))
  }

  if (var == "colrow") {
    newcol <- gxgy_to_var(.x, var = "rowcol", gridsize, plotdim)
    .x <- tibble::add_column(
      .x, 
      col = pad_dbl(newcol$col, width = 2, pad = 0), 
      row = pad_dbl(newcol$row, width = 2, pad = 0)
    )
    return(restore_add_var(.x, x))
  }

  if (var == "hectindex") {
    newcol <- gxgy_to_var(.x, var, gridsize = NULL, plotdim)
    .x <- tibble::add_column(.x, hectindex = newcol)
    return(restore_add_var(.x, x))
  }
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
add_quad <- function(x, gridsize = 20, plotdim = NULL, start = 1, width = 2) {
  stopifnot(start %in% c(0, 1))

  w_rowcol <- add_var(x, "colrow", gridsize = gridsize, plotdim = plotdim)
  if (start == 0) {
    w_rowcol$col <- as.numeric(w_rowcol$col) - 1
    w_rowcol$row <- as.numeric(w_rowcol$row) - 1
  }
  w_rowcol <- dplyr::mutate(
    w_rowcol,
    col = pad_dbl(col, width = width, pad = 0),
    row = pad_dbl(row, width = width, pad = 0),
    quad = paste0(col, row),
    row = NULL,
    col = NULL
  )
  w_rowcol
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
#' @noRd
restore_add_var <- function(.x, x) {
  .x <- restore_pxpy_if_necessary(.x, set_names(x, tolower))
  rename_matches(.x, x)
}

rename_pxpy <- function(x) {
  missing_names_gxgy <- !fgeo.tool::nms_has_any(x, "gx", "gy")
  has_names_pxpy <- fgeo.tool::nms_has_any(x, "px", "py")
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

#' @rdname add_var
#' @export
add_col_row2 <- function(x) {
  x <- add_var_from_quadratname(x, "^(..)..$", "col")
  x <- add_var_from_quadratname(x, "^..(..)$", "row")
  x
}

add_var_from_quadratname <- function(x, pattern, new_var) {
  if (!is.data.frame(x)) {
    abort("`x` must be a data.frame")
  }
  .x <- check_crucial_names(set_names(x, tolower), "quadratname")
  new_col <- set_names(data.frame(.x$quadratname), new_var)
  cbind(x, new_col)
}

