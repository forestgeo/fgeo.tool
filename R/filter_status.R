#' Filter a dataframe by the status of its each stem or each tree.
#'
#' @template x_fgeo
#' @param .status Character vector; Must be one of possible values of the
#'   variable giving the status of the dataframe `x`.
#' @param exclude Logical; `TRUE` excluded the values passed to `.status`.
#'
#' @return A filtered version of the dataframe `x`.
#' @export
#'
#' @examples
#' library(fgeo.utils)
#'
#' # Filter by the status of each stem (wood = "stem") -----------------------
#'
#' # Notice that the variable "status" (or Status) refers the each stem, not tree.
#'
#' # CENSUS TABLE: STEM TABLE
#'
#' x <- bciex::bci12s7mini
#' table(x$status)
#'
#' result <- filter_status(x, wood = "stem", .status = "D")
#' table(result$status)
#'
#' result <- filter_status(x, wood = "stem", .status = "D", exclude = TRUE)
#' table(result$status)
#' # Shortcut
#' result <- stem_not_dead(x)
#' table(result$status)
#'
#' # Warns
#' result <- filter_status(x, wood = "stem", .status = c("A", "wrong-status"))
#' table(result$status)
#'
#' # CENSUS TABLE: TREE TABLE
#'
#' # Works exactly in the same way
#' x <- bciex::bci12t7mini
#' table(x$status)
#'
#' result <- filter_status(x, wood = "stem", .status = "D")
#' table(result$status)
#'
#' # Shortcut
#' result <- stem_not_dead(x)
#' table(result$status)
#'
#' # VIEWFULL TABLE
#'
#' # Works exactly in the same way, but notice the following:
#' # * The variable Status starts with capital S;
#' # * The values of Status are not, say "A" or "D", but "alive" or "dead".
#' x <- bciex::bci12vft_mini
#' table(x$Status)
#'
#' result <- filter_status(x, wood = "stem", .status = "alive")
#' table(result$Status)
#'
#' # Warns because `.status` defaults to "D" -- not to "dead".
#' result <- stem_not_dead(x)
#' # Fix and repeat
#' result <- stem_not_dead(x, .status = "dead")
#' table(result$Status)
#'
#'
#'
#' # Filter by the status of each tree (wood = "tree") -----------------------
#'
#' # CENSUS TABLE: STEM TABLE
#'
#' x <- bciex::bci12s7mini
#'
#' # Add the variable status_tree, which gives the status of each tree, not stem
#' unique(x$status)
#' x <- add_status_tree(x, status_d = "D", status_a = "A")
#' table(x$status_tree)
#'
#' result <- filter_status(x, wood = "tree", .status = "A")
#' table(result$status_tree)
#'
#' result <- filter_status(x, wood = "tree", .status = "D", exclude = TRUE)
#' table(result$status_tree)
#'
#' # Shortcut
#' result <- tree_not_dead(x)
#' result %>%
#'   dplyr::arrange(tag, stemID, status) %>%
#'   dplyr::select(tag, stemID, status, status_tree)
#' table(result$status_tree)
#'
#'
#'
#' # CENSUS TABLE: TREE TABLE
#' x <- bciex::bci12t7mini
#' # For a tree census-table, each stem maps to a tree, so the value of the
#' # variable `status` gives, at the same time, the status of the stem and tree.
#' x <- add_status_tree(x, "D", "A")
#' identical(x$status, x$status_tree)
#' # So the result will be the same if we use `wood = tree` or `wood = stem`.
#' result1 <- filter_status(x, wood = "tree", .status = "A")
#' result2 <- filter_status(x, wood = "stem", .status = "A")
#' identical(result1, result2)
#' # Shortcut
#' result <- tree_not_dead(x, .status = "D")
#' identical(result, result1)
#'
#'
#'
#' # VIEWFULL TABLE
#'
#' x <- bciex::bci12vft_mini
#'
#' # Add the variable status_tree, which gives the status of each tree, not stem
#' unique(x$Status)
#' x <- add_status_tree(x, status_d = "dead", status_a = "alive")
#' table(x$status_tree)
#'
#' result <- filter_status(x, wood = "tree", .status = "alive")
#' table(result$status_tree)
#'
#' # Shortcut
#' unique(x$Status)
#' result <- tree_not_dead(x, "dead")
#' result %>%
#'   dplyr::arrange(Tag, StemID, Status) %>%
#'   dplyr::select(Tag, StemID, Status, status_tree)
#' table(result$status_tree)
filter_status <- function(x, wood, .status, exclude = FALSE) {
  old_nms <- names(x)
  x <- rlang::set_names(x, tolower)
  check_filter_status(x = x, wood = wood, .status = .status)

  if (wood == "stem") {
    stts_var <- x$status
  } else if (wood == "tree") {
    stts_var <- x$status_tree
  }

  if (exclude) {
    not_status <- setdiff(unique(stts_var), .status)
    filtered <- dplyr::filter(x, stts_var %in% not_status)
  } else {
    filtered <- dplyr::filter(x, stts_var %in% .status)
  }

  rlang::set_names(filtered, old_nms)
}

#' @rdname filter_status
#' @export
stem_not_dead <- function(x, .status = "D") {
  filter_status(x = x, wood = "stem", .status = .status, exclude = TRUE)
}

#' @rdname filter_status
#' @export
tree_not_dead <- function(x, .status = "D") {
  filter_status(x = x, wood = "tree", .status = .status, exclude = TRUE)
}

check_filter_status <- function(x, wood, .status) {
  stopifnot(is.data.frame(x))
  stopifnot(is.character(.status))
  stopifnot(is.character(wood))
  stopifnot(length(wood) == 1)
  stopifnot(wood  %in% c("stem", "tree"))
  if (wood == "stem") {
    check_valid_status(x, .status, "status")
  }
  if (wood == "tree") {
    check_valid_status(x, .status, "status_tree")
  }
}

check_valid_status <- function(x, .status, status_var) {
  .status_var <- x[[status_var]]
  check_crucial_names(x, status_var)
  valid_status <- unique(.status_var)
  invalid_status <- setdiff(.status, valid_status)
  if (length(invalid_status) != 0) {
    warning(
      "No observation has .status = ", collapse(invalid_status), "\n",
      "  * Valid values: ", collapse(valid_status)
    )
  }
}
