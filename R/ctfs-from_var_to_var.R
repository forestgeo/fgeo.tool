#' Functions to get variables from other variables.
#'
#' These functios wrap their corresponding functions from the CTFS R Package,
#' but these versions are stricter. The main differences are these:
#' * names use "_" not ".".
#' * argument gridsize defaults to missing to force the user to provide it.
#' * If the argument `plotdim` is missing from functions `gxgy_fun()`, its value
#'   will be guessed and notified.
#'
#' Best is to avoid these functions because some of them output a vector and
#' others output a dataframe. The recomended alternative to all these functions
#' is the wrapper [add_var()], which consistently inputs and outputs a
#' dataframe, and is formally tested.
#' 
#' `gxgy_to_qxqy()` didn't exist in the original CTFS R Package. Added for
#' consistency.
#'
#' @template gxgy
#' @template gridsize
#' @template plotdim
#' @template rowno_colno
#' @template index
#'
#' @seealso [add_var()].
#' @return A vector or dataframe (see examples).
#'
#' @examples
#' \dontrun{
#' gxgy_to_index(c(0, 400, 990), c(0, 200, 490), gridsize = 20)
#'
#' gridsize <- 20
#' plotdim <- c(1000, 500)
#'
#' x <- gxgy_to_hectindex(1:3, 1:3, plotdim)
#' x
#' typeof(x)
#' is.data.frame(x)
#' is.vector(x)
#'
#' x <- gxgy_to_index(1:3, 1:3, gridsize, plotdim)
#' x
#' typeof(x)
#' is.data.frame(x)
#' is.vector(x)
#'
#' x <- gxgy_to_lxly(1:3, 1:3, gridsize, plotdim)
#' x
#' typeof(x)
#' is.data.frame(x)
#' is.vector(x)
#'
#' x <- gxgy_to_rowcol(1:3, 1:3, gridsize, plotdim)
#' x
#' typeof(x)
#' is.data.frame(x)
#' is.vector(x)
#'
#' x <- index_to_rowcol(1:3, gridsize, plotdim)
#' x
#' typeof(x)
#' is.data.frame(x)
#' is.vector(x)
#'
#' x <- rowcol_to_index(1:3, 1:3, gridsize, plotdim)
#' x
#' typeof(x)
#' is.data.frame(x)
#' is.vector(x)
#' }
#' 
#' index_to_gxgy(1:3, gridsize, plotdim)
#' @name from_var_to_var
#' @noRd
NULL

#' @rdname from_var_to_var
#' @noRd
rowcol_to_index <- function(rowno, colno, gridsize, plotdim) {
  badrc <- (rowno <= 0 | colno <= 0 | rowno > plotdim[2] / gridsize |
      colno > plotdim[1] / gridsize)
  rowno <- rowno - 1
  colno <- colno - 1
  maxrow <- floor(plotdim[2] / gridsize)
  index <- colno * maxrow + rowno + 1
  if (length(badrc[badrc > 0])) {
    index[badrc] <- NA
  }
  return(index)
}

#' @rdname from_var_to_var
#' @noRd
index_to_rowcol <- function(index, gridsize, plotdim) {
  index <- index - 1
  badindex <- (index < 0 | index >= plotdim[1] * plotdim[2] / (gridsize ^ 2))
  maxrow <- floor(plotdim[2] / gridsize)
  rowno <- index %% maxrow
  colno <- floor((index - rowno) / maxrow)
  row <- rowno + 1
  col <- colno + 1
  if (length(badindex[badindex > 0])) {
    row[badindex] <- col[badindex] <- -1
  }
  return(data.frame(row = row, col = col))
}

#' @rdname from_var_to_var
#' @noRd
gxgy_to_index <- function(gx, gy, gridsize, plotdim) {
  if (missing(plotdim)) {
    plotdim <- fgeo.base::guess_plotdim(tibble(gx = gx, gy = gy))
  }

  badgxgy <- (gx < 0 | gy < 0 | gx >= plotdim[1] | gy >= plotdim[2] |
    is.na(gx) | is.na(gy))
  colno <- 1 + floor(gx / gridsize)
  rowno <- 1 + floor(gy / gridsize)
  if (length(badgxgy[badgxgy > 0])) {
    colno[badgxgy] <- rowno[badgxgy] <- NA
  }
  return(rowcol_to_index(rowno, colno, gridsize, plotdim))
}

#' @rdname from_var_to_var
#' @noRd
gxgy_to_lxly <- function(gx, gy, gridsize, plotdim) {
  if (missing(plotdim)) {
    plotdim <- fgeo.base::guess_plotdim(tibble(gx, gy))
  }

  rc <- gxgy_to_rowcol(gx, gy, gridsize, plotdim) - 1
  lx <- gx - gridsize * rc$col
  ly <- gy - gridsize * rc$row
  return(data.frame(lx, ly))
}

#' @rdname from_var_to_var
#' @noRd
gxgy_to_qxqy <- function(gx, gy, gridsize, plotdim) {
  lxly <- gxgy_to_lxly(gx = gx, gy = gy, gridsize = gridsize, plotdim = plotdim)
  dplyr::rename(lxly, QX = .data$lx, QY = .data$ly)
}

#' @rdname from_var_to_var
#' @noRd
gxgy_to_rowcol <- function(gx, gy, gridsize, plotdim) {
  if (missing(plotdim)) {
    plotdim <- fgeo.base::guess_plotdim(tibble(gx, gy))
  }

  index <- gxgy_to_index(gx, gy, gridsize, plotdim)
  return(index_to_rowcol(index, gridsize, plotdim))
}

#' @rdname from_var_to_var
#' @noRd
gxgy_to_hectindex <- function(gx, gy, plotdim) {
  if (missing(plotdim)) {
    plotdim <- fgeo.base::guess_plotdim(tibble(gx = gx, gy = gy))
  }

  if (gx >= plotdim[1] || gy >= plotdim[2] || gx < 0 || gy < 0) {
    stop("Some values of `gx` and/or `gy` outsite the census plot\n")
  } else {
    ha.rowno <- floor(gy / 100)
    ha.colno <- floor(gx / 100)
    max.ha.row <- plotdim[2] / 100
    return(ha.colno * max.ha.row + ha.rowno + 1)
  }
}

#' @rdname from_var_to_var
#' @noRd
index_to_gxgy <- function(index, gridsize, plotdim) {
  badindex <- (index <= 0 | index > plotdim[1] * plotdim[2] / (gridsize ^ 2))
  rc <- index_to_rowcol(index, gridsize, plotdim)
  gx <- gridsize * (rc$col - 1)
  gy <- gridsize * (rc$row - 1)
  if (length(badindex[badindex > 0])) {
    gx[badindex] <- (-1)
    gy[badindex] <- (-1)
  }
  data.frame(gx = gx, gy = gy)
}
