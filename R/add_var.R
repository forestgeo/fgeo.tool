#' Add columns lx, ly; qx, qy; index; colrow; and hectindex to a ForestGEO dataframe.
#' 
#' These functions add columns to position trees/stems in a forest plot. They 
#' work with ViewFull-tables and census-tables (tree and stem).
#'
#' These functions mostly wrap legacy code from the [CTFS R
#' Package](http://ctfs.si.edu/Public/CTFSRPackage/).
#'
#' @template x_fgeo
#' @template gridsize
#' @template plotdim
#' @param start `1` or `0`, indicating how to label the first plot-column.
#' @param width Number; width to pad the labels of plot-columns and -rows
#'   (passed to [stringr::str_pad()]).
#'
#' @family functions to add columns to dataframes.
#' @family functions for ForestGEO data.
#' @family functions for fgeo census.
#' @family functions for fgeo vft.
#' @seealso [stringr::str_pad()].
#'
#' @return A modified version of the dataframe `x` with the additional
#'   variable(s) `var`.
#'
#' @examples
#' x <- tibble::tibble(gx = c(0, 50, 999.9, 1000), gy = gx/2)
#'
#' add_lxly(x)
#' 
#' add_qxqy(x)
#'
#' add_index(x)
#'
#' add_hectindex(x)
#'
#' add_quad(x)
#' add_quad(x, start = 0)
#'
#' # `width` gives the nuber of digits to pad the label of plot-rows and
#' # plot-columns, e.g. 3 pads plot-rows with three zeros and plot-columns with an
#' # extra trhree zeros, resulting in a total of 6 zeros.
#' add_quad(x, start = 0, width = 3)
#'
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
#' @name add_var
NULL

# FIXME: Flatten if statements via switch or functionals.
add_var <- function(x, var, gridsize = 20, plotdim = NULL) {
  xlow <- set_names(x, tolower)
  .x <- sanitize_xy(xlow)
  
  check_add_var(x = .x, var = var, gridsize = gridsize, plotdim = plotdim)

  if (is.null(plotdim)) {
    plotdim <- plotdim
    plotdim <- fgeo.base::guess_plotdim(.x)
    message("  * If guess is wrong, provide the correct argument `plotdim`")
  }

  if (var == "lxly") {
    lxly <- gxgy_to_lxly(.x$gx, .x$gy, gridsize = gridsize, plotdim = plotdim)
    out <- tibble::add_column(.x, lx = lxly$lx, ly = lxly$ly)
    out <- restore_pxpy_if_necessary(out, xlow)
    out <- rename_matches(out, x)
    return(out)
  }

  if (var == "qxqy") {
    lxly <- gxgy_to_lxly(.x$gx, .x$gy, gridsize = gridsize, plotdim = plotdim)
    out <- tibble::add_column(.x, QX = lxly$lx, QY = lxly$ly)
    out <- restore_pxpy_if_necessary(out, xlow)
    out <- rename_matches(out, x)
    return(out)
  }

  if (var == "index") {
    index <- gxgy_to_index(.x$gx, .x$gy, gridsize = gridsize, plotdim = plotdim)
    out <- tibble::add_column(.x, index = index)
    out <- restore_pxpy_if_necessary(out, xlow)
    out <- rename_matches(out, x)
    return(out)
  }

  if (var == "colrow") {
    rowcol <- gxgy_to_rowcol(.x$gx, .x$gy, gridsize = gridsize, plotdim = plotdim)
    out <- tibble::add_column(
      .x, 
      col = stringr::str_pad(rowcol$col, width = 2, pad = 0), 
      row = stringr::str_pad(rowcol$row, width = 2, pad = 0)
    )
    out <- restore_pxpy_if_necessary(out, xlow)
    out <- rename_matches(out, x)
    return(out)
  }

  if (var == "hectindex") {
    w_hectindex <- gxgy_to_hectindex(.x$gx, .x$gy, plotdim = plotdim)
    out <- tibble::add_column(.x, hectindex = w_hectindex)
    out <- restore_pxpy_if_necessary(out, xlow)
    out <- rename_matches(out, x)
    return(out)
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
    col = stringr::str_pad(col, width = width, pad = 0),
    row = stringr::str_pad(row, width = width, pad = 0),
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

rename_pxpy <- function(x) {
  missing_names_gxgy <- !fgeo.tool::nms_has_any(x, "gx", "gy")
  has_names_pxpy <- fgeo.tool::nms_has_any(x, "px", "py")
  missing_names_gxgy && has_names_pxpy
}


#' Restore px py columns if they were renamed to gx gy.
#'
#' @param x A dataframe to rename if necessary. 
#' @param ref The reference dataframe that needs to be checked to see if
#'   renaming is necessary.
#' @noRd
restore_pxpy_if_necessary <- function(x, ref) {
  if (rename_pxpy(ref)) {
    x <- dplyr::rename(x, px = gx, py = gy)
  }
  x
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

