#' Add columns to a dataframe.
#'
#' These functions mostly wrap legacy code from the [CTFS R
#' Package](http://ctfs.si.edu/Public/CTFSRPackage/). The functions 
#' `add_col_row()` and `add_col_row2()` differ in that the former inputs 
#' plot coordinates and the later inputs `QuadratName`.
#'
#' @template x_fgeo
#' @param var A character string; either "lxly", "colrow", "index", hectindex.
#' @template gridsize
#' @template plotdim
#' @param start `1` or `0`, indicating how to label the first plot-column.
#' @param width Number; width to padd the labels of plot-columns and -rows
#'   (passed to [stringr::str_pad()]).
#'
#' @family functions to add columns to dataframes.
#' @seealso [stringr::str_pad()].
#'
#' @return A modified version of the dataframe `x` with the additional
#'   variable(s) `var`.
#' @export
#'
#' @examples
#' x <- tibble::tibble(gx = c(0, 50, 999.9, 1000), gy = gx/2)
#'
#' # Each `add_var(x, var = "*")` has a shortcut: `add_*()`
#' add_var(x, var = "lxly")
#' # same
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
add_var <- function(x, var, gridsize = 20, plotdim = NULL) {
  check_add_var(x = x, var = var, gridsize = gridsize, plotdim = plotdim)

  if (is.null(plotdim)) {
    plotdim <- plotdim
    plotdim <- guess_plotdim(x)
    message("  * If guess is wrong, provide the correct argument `plotdim`")
  }

  if (var == "lxly") {
    lxly <- gxgy_to_lxly(x$gx, x$gy, gridsize = gridsize, plotdim = plotdim)
    return(tibble::add_column(x, lx = lxly$lx, ly = lxly$ly))
  }

  if (var == "qxqy") {
    lxly <- gxgy_to_lxly(x$gx, x$gy, gridsize = gridsize, plotdim = plotdim)
    return(tibble::add_column(x, QX = lxly$lx, QY = lxly$ly))
  }

  if (var == "index") {
    index <- gxgy_to_index(x$gx, x$gy, gridsize = gridsize, plotdim = plotdim)
    return(tibble::add_column(x, index = index))
  }

  if (var == "colrow") {
    rowcol <- gxgy_to_rowcol(x$gx, x$gy, gridsize = gridsize, plotdim = plotdim)
    x <- tibble::add_column(
      x, 
      col = stringr::str_pad(rowcol$col, width = 2, pad = 0), 
      row = stringr::str_pad(rowcol$row, width = 2, pad = 0)
    )
    return(x)
  }

  if (var == "hectindex") {
    w_hectindex <- gxgy_to_hectindex(x$gx, x$gy, plotdim = plotdim)
    return(tibble::add_column(x, hectindex = w_hectindex))
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
    rlang::abort("`x` must be a data.frame")
  }
  old_nms <- names(x)
  x <- set_names(x, tolower)
  fgeo.tool::check_crucial_names(x, "quadratname")
  
  x$added_var <- stringr::str_replace(x$quadratname, pattern, "\\1")
  names(x)[grepl("added_var", names(x))] <- new_var
  fgeo.tool::nms_restore_newvar(x, new_var = new_var, old_nms = old_nms)
}









#' Guess plot dimensions.
#'
#' @template x_fgeo
#' @param accuracy A number giving the accuracy with which to round `gx` and
#'   `gy`.
#'
#' @return A numeric vector of length 2.
#' @export
#'
#' @examples
#' x <- data.frame(
#'   gx = c(0, 300, 980),
#'   gy = c(0, 300, 499)
#' )
#' guess_plotdim(x)
guess_plotdim <- function(x, accuracy = 50) {
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(accuracy))
  check_crucial_names(x, c("gx", "gy"))
  
  guess <- purrr::map_dbl(x[ , c("gx", "gy")], guess_max, accuracy = accuracy)
  message("Gessing: plotdim = c(", commas(guess), ")")
  guess <- unname(guess)
}
guess_max <- function(x, ...) {
  xmax <- max0(x)
  plyr::round_any(xmax, f = ceiling, ...)
}
