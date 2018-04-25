commas <- function(...) {
  paste0(..., collapse = ", ")
}

max0 <- function(...) {
  max(..., na.rm = TRUE)
}

min0 <- function(...) {
  min(..., na.rm = TRUE)
}



has_class_df <- function(x) {
  any(grepl("data.frame", class(x)))
}

each_list_item_is_df <- function(x) {
  if (!is.list(x) || is.data.frame(x)) {
    abort("`x` must be a list of datafraems (and not itself a dataframe).")
  }
  all(purrr::map_lgl(x, has_class_df))
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
