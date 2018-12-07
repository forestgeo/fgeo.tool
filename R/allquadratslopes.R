#' Calculates the slope of all quadrats in a plot.
#'
#' @details
#' `allquadratslopes()` goes through all 20x20 m quadrats in a plot and finds
#' the slope, mean elevation, and convexity of each. Convexity is the mean
#' elevation of one 20x20 m quadrat relative (minus) the mean of its immediate
#' neighbors.
#'
#' Helene Muller-Landau added a section to correct convexity in edge quadrats.
#'
#' @param plotdim The x and y dimensions of the plot.
#' @param gridsize Side of the square quadrat.
#' @param elev A list named `col` containing a data.frame with variables `x`,
#'   `y` and `elev`. This object is very specific; See example.
#' @param edgecorrect Correct convexity in edge quadrats?
#'
#' @seealso [calcslope()], [quadslope()]
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#'
#' @author Rick Condit, Suzanne Lao.
#'
#' @export
#' @keywords internal
#' @examples
#' # The input to elev is very specific; you may need to tweak it.
#' elev <- fgeo.x::elevation
#' result <- allquadratslopes(
#'   elev = elev,
#'   gridsize = 20,
#'   plotdim = c(1000, 500),
#'   edgecorrect = TRUE
#' )
#' head(result)
#' str(result)
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

  elevdata <-
    elev$col[elev$col$x %% gridsize == 0 & elev$col$y %% gridsize == 0, ]
  elevmat <- matrix(elevdata$elev, nrow = rows, ncol = columns, byrow = F)
  meanelev <- convex <- slope <- numeric()
  corner <- numeric()
  for (c in 1:(columns - 1)) for (r in 1:(rows - 1)) {
      quad_idx <- rowcol_to_index(r, c, gridsize = gridsize, plotdim = plotdim)
      corner[1] <- elevmat[r, c]
      corner[2] <- elevmat[r + 1, c]
      corner[3] <- elevmat[r + 1, c + 1]
      corner[4] <- elevmat[r, c + 1]
      meanelev[quad_idx] <- mean(corner)
      slope[quad_idx] <- quadslope(corner, gridsize = gridsize)[1]
      if (c %% 33 == 0 && r %% 33 == 0) {
        message("Finding elevation and slope of quadrat ", quad_idx, "\n")
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

  warn_if_no_data_falls_on_half_gridsize(elev, gridsize, edgecorrect)
  # I'm conserving the flow of legacy code. I let the function continue past the
  # warning. The warning should be informative enought to let the user know what
  # to do.

  if (edgecorrect) {
    for (c in 1:(columns - 1)) for (r in 1:(rows - 1)) {
      first_or_prevlast_col <- (c == 1) || (c == (columns - 1))
      first_or_prevlast_row <-  (r == 1) || (r == (rows - 1))
        if (first_or_prevlast_col || first_or_prevlast_row) {
          quad_idx <- rowcol_to_index(
            r, c, gridsize = gridsize, plotdim = plotdim
          )
          xy <- index_to_gxgy(quad_idx, gridsize = gridsize, plotdim = plotdim)
          midx <- xy$gx + gridsize / 2
          midy <- xy$gy + gridsize / 2

          xy_on_midpoint <- elev$col$x == midx & elev$col$y == midy
          elevcol <- elev$col[xy_on_midpoint, , drop = FALSE]
          midelev <- elevcol$elev
          convex[quad_idx] <- midelev - meanelev[quad_idx]
        }
      }
  }

  data.frame(meanelev = meanelev, convex = convex, slope = slope)
}

warn_if_no_data_falls_on_half_gridsize <- function(elev, gridsize, edgecorrect) {
  midpoint <- gridsize / 2
  data_on_half_gridsize <- elev$col$x == midpoint & elev$col$y == midpoint
  if (edgecorrect && !any(data_on_half_gridsize)) {
    msg <- paste0(
      "No elevation data found at `gridsize / 2`.\n",
      " * Is your elevation data too coarse?\n",
      "* Do you need to use `edgecorrect = FALSE`?\n"
    )
    warning(msg, call. = FALSE)
  }
}

#' Internal.
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @author Rick Condit, Suzanne Lao.
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
#' @author Rick Condit, Suzanne Lao.
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
#' @author Rick Condit, Suzanne Lao.
calcslope <- function(z, gridsize) {
  if (!equal(length(z), 3)) {
    stop("`z` must be of length 3, not ", length(z), ".", call. = FALSE)
  }

  z2 <- z[3] - z[2]
  z1 <- z[1] - z[2]
  if (equal(z2, 0)) {
    if (equal(z1, 0)) {
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

equal <- function(x, y) isTRUE(all.equal(x, y))
