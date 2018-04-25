#' Create habitat data from elevation data.
#'
#' @param elevation List containing elevation data.
#' @param gridsize Number to round `x` and `y` by. Commonly, `gridsize = 20`.
#' @param n Number of elevation groups (habitats) to cut elevation data by. 
#' Commonly, `n = 4`.
#'
#' @return A dataframe of habitat data.
#' @export
#'
#' @examples
#' create_habitat(fgeo.data::luquillo_elevation)
create_habitat <- function(elevation, gridsize = 20, n = 4) {
  check_crucial_names(elevation, c("col", "xdim", "ydim"))
  
  elevation$col %>%
    dplyr::as_tibble() %>% 
    dplyr::mutate(
      x = round_any(x, gridsize),
      y = round_any(y, gridsize)
    ) %>% 
    unique() %>% 
    dplyr::group_by(x, y) %>%
    dplyr::summarise(elev = mean(elev)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      habitats = as.integer(cut_number(elev, n)),
      elev = NULL
    ) %>% 
    dplyr::filter(x < elevation$xdim, y < elevation$ydim)
}

#' Round to multiple of any number. Copied from `plyr:::round_any.numeric()`.
#' 
#' @param x Numeric vector to round.
#' @param accuracy Number to round to.
#' @param f Rounding function: floor, ceiling or round.
#' 
#' @seealso `plyr::round_any()` and \url{http://bit.ly/2JrBQK3}.
#' 
#' @noRd
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

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
