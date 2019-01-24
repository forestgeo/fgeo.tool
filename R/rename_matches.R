#' Rename an object based on case-insensitive match of the names of a reference.
#'
#' @param y Named object to use as reference.
#' @param x x object which names to restored if they match the reference.
#'
#' @return The output is `x` with as many names changed as case-insensitive
#'   matches there are with the reference.
#'
#' @examples
#' ref <- data.frame(COL1 = 1, COL2 = 1)
#' x <- data.frame(col1 = 5, col2 = 1, n = 5)
#' rename_matches(x, ref)
#' @family functions for developers
#' @family general functions to deal with names
#' @keywords internal
#' @export
rename_matches <- function(x, y) {
  names(x) <- extract_insensitive(names(x), names(y))
  x
}

#' Detect and extract matching strings -- ignoring case.
#'
#' @param x A string to be muted as in `y`, it a case insensitive match is
#'   found.
#' @param y A string to use as a reference to match `x`.
#'
#' @return `detect_*` and `extract_*` return a logical vector and a string.
#'
#' @examples
#' x <- c("stemid", "n")
#' y <- c("StemID", "treeID")
#' detect_insensitive(x, y)
#' extract_insensitive(x, y)
#' 
#' vft <- data.frame(TreeID = 1, Status = 1)
#' extract_insensitive(tolower(names(vft)), names(vft))
#' extract_insensitive(names(vft), tolower(names(vft)))
#' @family functions for developers
#' @family general functions to deal with names
#' @keywords internal
#' @export
extract_insensitive <- function(x, y) {
  x <- as.character(x)
  y <- as.character(y)

  # Is the element of x in y?
  pull_replacement <- function(x, y) {
    stopifnot(length(x) == 1)
    if (is.na(x)) {
      return(x)
    }

    in_x <- detect_insensitive(y, x)
    replacement <- y[in_x]
    if (length(replacement) == 0) {
      return(x)
    }

    unique(replacement)
  }
  unname(vapply(x, pull_replacement, character(1), y))
}

#' Return TRUE in position where name of x is in y; FALSE otherwise.
#'
#' @rdname extract_insensitive
#' @export
detect_insensitive <- function(x, y) {
  stopifnot(is.character(x), is.character(y))
  matches <- lapply(anchor(x), grepl, y, ignore.case = TRUE)
  vapply(matches, any, logical(1))
}
