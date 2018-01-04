#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
gxgy.to.index <- function(gx, gy, gridsize = 20, plotdim = c(1000, 500)) {
  badgxgy <- (gx < 0 | gy < 0 | gx >= plotdim[1] | gy >= plotdim[2] |
    is.na(gx) | is.na(gy))
  colno <- 1 + floor(gx / gridsize)
  rowno <- 1 + floor(gy / gridsize)
  if (length(badgxgy[badgxgy > 0])) {
    colno[badgxgy] <- rowno[badgxgy] <- NA
  }
  return(rowcol.to.index(rowno, colno, gridsize, plotdim))
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
gxgy.to.lxly <- function(gx, gy, gridsize = 20, plotdim = c(1000, 500)) {
  rc <- gxgy.to.rowcol(gx, gy, gridsize, plotdim) - 1
  lx <- gx - gridsize * rc$col
  ly <- gy - gridsize * rc$row
  return(data.frame(lx, ly))
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
gxgy.to.rowcol <- function(gx, gy, gridsize = 20, plotdim = c(1000, 500)) {
  index <- gxgy.to.index(gx, gy, gridsize, plotdim)
  return(index.to.rowcol(index, gridsize, plotdim))
}



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
rowcol.to.index <- function(rowno, colno, gridsize = 20, plotdim = c(1000, 500)) {
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



#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
index.to.rowcol <- function(index, gridsize = 20, plotdim = c(1000, 500)) {
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
