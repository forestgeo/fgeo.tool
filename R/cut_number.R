# From ggplot2 ------------------------------------------------------------

#' Copied from `ggplot2::cut_number`.
#' 
#' @param x Numeric vector.
#' @param x Numeric vector.
#' @param n Number of intervals to create.
#' @param ... Other arguments passed on to cut.
#' 
#' @seealso `ggplot2::cut_number()`, \url{http://bit.ly/2qUGMAk}.
#' 
#' @noRd
cut_number <- function(x, n = NULL, ...) {
  brk <- breaks(x, "n", n)
  if (anyDuplicated(brk)) 
    stop("Insufficient data values to produce ", n, " bins.", 
      call. = FALSE)
  cut(x, brk, include.lowest = TRUE, ...)
}

breaks <- function (x, equal, nbins = NULL, binwidth = NULL) {
  equal <- match.arg(equal, c("numbers", "width"))
  if ((!is.null(nbins) && !is.null(binwidth)) || (is.null(nbins) && 
      is.null(binwidth))) {
    stop("Specify exactly one of n and width")
  }
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  if (equal == "width") {
    if (!is.null(binwidth)) {
      fullseq(rng, binwidth)
    }
    else {
      seq(rng[1], rng[2], length.out = nbins + 1)
    }
  }
  else {
    if (!is.null(binwidth)) {
      probs <- seq(0, 1, by = binwidth)
    }
    else {
      probs <- seq(0, 1, length.out = nbins + 1)
    }
    stats::quantile(x, probs, na.rm = TRUE)
  }
}

# From scales -------------------------------------------------------------

#' Copy of scales::fullseq.
#' @noRd
fullseq <- function (range, size, ..., pad = FALSE) {
  if (zero_range(range)) 
    return(range + size * c(-1, 1)/2)
  x <- seq(
    fgeo.base::round_any(range[1], size, floor),
    fgeo.base::round_any(range[2], size, ceiling), 
    by = size
  )
  if (pad) {
    c(min(x) - size, x, max(x) + size)
  }
  else {
    x
  }
}

#' Copy of scales::zero_range().
#' @noRd
zero_range <- function (x, tol = 1000 * .Machine$double.eps) {
  if (length(x) == 1) 
    return(TRUE)
  if (length(x) != 2) 
    stop("x must be length 1 or 2")
  if (any(is.na(x))) 
    return(NA)
  if (x[1] == x[2]) 
    return(TRUE)
  if (all(is.infinite(x))) 
    return(FALSE)
  m <- min(abs(x))
  if (m == 0) 
    return(FALSE)
  abs((x[1] - x[2])/m) < tol
}
