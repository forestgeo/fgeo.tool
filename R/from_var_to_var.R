#' Functions to get variables from other variables.
#'
#' These functions wrap their corresponding functions from the CTFS R Package,
#' but these versions are stricter. The main differences are these:
#' * names use "_" not ".".
#' * argument gridsize defaults to missing to force the user to provide it.
#' * If the argument `plotdim` is missing from functions `gxgy_fun()`, its value
#'   will be guessed and notified.
#'
#' `gxgy_to_qxqy()` didn't exist in the original CTFS R Package. Added for
#' consistency.
#'
#' @param gx,gy A number; global x and y position in a census plot.
#' @param gridsize The gridsize of the census plot (commonly 20 m).
#' @param plotdim The global dimensions of the census plot (i.e. the
#'   maximum possible values of `gx` and `gy`).
#' @param rowno,colno Row and column number -- as defined in a census plot.
#' @param index Index number as defined for a census plot.
#'
#' @return A vector or dataframe (see examples).
#'
#' @author Rick Condit, Suzanne Lao.
#' @examples
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
#' 
#' index_to_gxgy(1:3, gridsize, plotdim)
#' @keywords internal
#' @name from_var_to_var
NULL

#' @rdname from_var_to_var
#' @export
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

  index
}

#' @rdname from_var_to_var
#' @export
index_to_rowcol <- function(index, gridsize, plotdim) {
  index <- index - 1
  badindex <- (index < 0 | index >= plotdim[1] * plotdim[2] / (gridsize^2))
  maxrow <- floor(plotdim[2] / gridsize)
  rowno <- index %% maxrow
  colno <- floor((index - rowno) / maxrow)
  row <- rowno + 1
  col <- colno + 1
  if (length(badindex[badindex > 0])) {
    row[badindex] <- col[badindex] <- -1
  }

  data.frame(row = row, col = col)
}

#' @rdname from_var_to_var
#' @export
gxgy_to_index <- function(gx, gy, gridsize, plotdim) {
  if (missing(plotdim)) {
    plotdim <- guess_plotdim(tibble(gx = gx, gy = gy))
  }

  badgxgy <- (gx < 0 | gy < 0 | gx >= plotdim[1] | gy >= plotdim[2] |
    is.na(gx) | is.na(gy))
  colno <- 1 + floor(gx / gridsize)
  rowno <- 1 + floor(gy / gridsize)
  if (length(badgxgy[badgxgy > 0])) {
    colno[badgxgy] <- rowno[badgxgy] <- NA
  }

  rowcol_to_index(rowno, colno, gridsize, plotdim)
}

#' @rdname from_var_to_var
#' @export
gxgy_to_lxly <- function(gx, gy, gridsize, plotdim) {
  if (missing(plotdim)) {
    plotdim <- guess_plotdim(tibble(gx, gy))
  }

  rc <- gxgy_to_rowcol(gx, gy, gridsize, plotdim) - 1
  lx <- gx - gridsize * rc$col
  ly <- gy - gridsize * rc$row

  data.frame(lx, ly)
}

#' @rdname from_var_to_var
#' @export
gxgy_to_qxqy <- function(gx, gy, gridsize, plotdim) {
  lxly <- gxgy_to_lxly(gx = gx, gy = gy, gridsize = gridsize, plotdim = plotdim)
  names(lxly)[grepl("lx", names(lxly))] <- "QX"
  names(lxly)[grepl("ly", names(lxly))] <- "QY"

  lxly
}

#' @rdname from_var_to_var
#' @export
gxgy_to_rowcol <- function(gx, gy, gridsize, plotdim) {
  if (missing(plotdim)) {
    plotdim <- guess_plotdim(tibble(gx, gy))
  }

  index <- gxgy_to_index(gx, gy, gridsize, plotdim)
  index_to_rowcol(index, gridsize, plotdim)
}

#' @rdname from_var_to_var
#' @export
gxgy_to_hectindex <- function(gx, gy, plotdim) {
  if (missing(plotdim)) {
    plotdim <- guess_plotdim(tibble(gx = gx, gy = gy))
  }

  if (at_or_beyond_edge(gx, gy, plotdim)) {
    warn("Some values of `gx` and/or `gy` lay at or beyond plot limits\n")
  }

  ha.rowno <- floor(gy / 100)
  ha.colno <- floor(gx / 100)
  max.ha.row <- plotdim[2] / 100

  ha.colno * max.ha.row + ha.rowno + 1
}

at_or_beyond_edge <- function(gx, gy, plotdim) {
  out <-
    any(gx >= plotdim[1]) || any(gy >= plotdim[2]) || any(gx < 0) || any(gy < 0)
  if (is.na(out)) {
    out <- FALSE
  }

  out
}

#' @rdname from_var_to_var
#' @export
index_to_gxgy <- function(index, gridsize, plotdim) {
  badindex <- (index <= 0 | index > plotdim[1] * plotdim[2] / (gridsize^2))
  rc <- index_to_rowcol(index, gridsize, plotdim)
  gx <- gridsize * (rc$col - 1)
  gy <- gridsize * (rc$row - 1)
  if (length(badindex[badindex > 0])) {
    gx[badindex] <- (-1)
    gy[badindex] <- (-1)
  }

  data.frame(gx = gx, gy = gy)
}
