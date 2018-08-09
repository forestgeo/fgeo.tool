#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
allquadratslopes <- function(elev, gridsize, plotdim, edgecorrect = TRUE) {
  if (!"col" %in% names(elev)) {
    warning("Input to elev must be a list with one element named 'col'.")
  }
  rw <- cl <- 0
  on.exit(message(rw, " ", cl, "\n"))
  columns <- 1 + max(elev$col$x) / gridsize
  rows <- 1 + max(elev$col$y) / gridsize
  totalquads <- (columns - 1) * (rows - 1)
  message("Calculating topographic indices for ", totalquads, " quadrats\n")
  elevdata <- elev$col[elev$col$x %% gridsize == 0 & elev$col$y %% gridsize ==
    0, ]
  elevmat <- matrix(elevdata$elev, nrow = rows, ncol = columns, byrow = F)
  meanelev <- convex <- convex2 <- slope <- numeric()
  corner <- sideht <- numeric()
  for (c in 1:(columns - 1)) for (r in 1:(rows - 1)) {
      quad.index <- rowcol_to_index(r, c, gridsize = gridsize, plotdim = plotdim)
      corner[1] <- elevmat[r, c]
      corner[2] <- elevmat[r + 1, c]
      corner[3] <- elevmat[r + 1, c + 1]
      corner[4] <- elevmat[r, c + 1]
      meanelev[quad.index] <- mean(corner)
      slope[quad.index] <- quadslope(corner, gridsize = gridsize)[1]
      if (c %% 33 == 0 & r %% 33 == 0) {
        message("Finding elevation and slope of quadrat ", quad.index, "\n")
      }
    }
  for (i in 1:totalquads) {
    neighbor.quads <- findborderquads(
      i, dist = gridsize, gridsize = gridsize, plotdim = plotdim
    )
    meanelev.neighbor <- mean(meanelev[neighbor.quads])
    convex[i] <- meanelev[i] - meanelev.neighbor
    if (i %% 1000 == 0) {
      message("Finding convexity of quadrat ", i, "\n")
    }
  }
  if (edgecorrect) {
    for (c in 1:(columns - 1)) for (r in 1:(rows - 1)) {
        if ((c == 1) | (c == (columns - 1)) | (r == 1) |
          (r == (rows - 1))) {
          quad.index <- rowcol_to_index(r, c,
            gridsize = gridsize,
            plotdim = plotdim
          )
          xy <- index_to_gxgy(quad.index,
            gridsize = gridsize,
            plotdim = plotdim
          )
          midx <- xy$gx + gridsize / 2
          midy <- xy$gy + gridsize / 2
          cond_1 <- elev$col$x == midx & elev$col$y ==
            midy
          elevcol <- elev$col[cond_1, , drop = FALSE]
          midelev <- elevcol$elev
          convex[quad.index] <- midelev - meanelev[quad.index]
        }
      }
  }
  return(data.frame(meanelev = meanelev, convex = convex, slope = slope))
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
findborderquads <- function(index, dist, gridsize, plotdim) {
  bound.index <- numeric(8)
  no.boundaries <- 0
  row <- index_to_rowcol(index, gridsize, plotdim)$row
  col <- index_to_rowcol(index, gridsize, plotdim)$col
  maxrow <- plotdim[2] / gridsize
  maxcol <- plotdim[1] / gridsize
  layers <- floor(dist / gridsize)
  for (i in (row - layers):(row + layers)) for (j in (col -
      layers):(col + layers)) if (i != row | j != col) {
        if (i >= 1 & i <= maxrow & j >= 1 & j <= maxcol) {
          no.boundaries <- no.boundaries + 1
          bound.index[no.boundaries] <- rowcol_to_index(
            i, j,
            gridsize, plotdim
          )
        }
      }
  return(bound.index[bound.index > 0])
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
quadslope <- function(cornerelev, gridsize) {
  slope <- numeric(4)
  z <- numeric(3)
  for (j in 1:4) {
    post <- 1
    for (k in (j + 1):(j + 3)) {
      if (k > 4) {
        m <- k %% 4
      } else {
        m <- k
      }
      z[post] <- cornerelev[m]
      post <- post + 1
    }
    slope[j] <- calcslope(z, gridsize)
  }
  return(c(mean(slope), sqrt(stats::var(slope))))
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
calcslope <- function(z, gridsize) {
  z2 <- z[3] - z[2]
  z1 <- z[1] - z[2]
  if (z2 == 0) {
    if (z1 == 0) {
      return(0)
    } else {
      denom <- sqrt(1 + (gridsize / z1)^2)
    }
    theta1 <- acos((-gridsize / z1) / denom)
    theta2 <- acos((gridsize / z1) / denom)
  }
  else {
    denom <- sqrt(1 + (z1 / z2)^2 + (gridsize / z2)^2)
    theta1 <- acos((-gridsize / z2) / denom)
    theta2 <- acos((gridsize / z2) / denom)
  }
  if (theta1 <= theta2) {
    return(180 * theta1 / pi)
  } else {
    return(180 * theta2 / pi)
  }
}
