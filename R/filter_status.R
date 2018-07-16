#' Filter a (fgeo) dataframe by the status of each stem or tree.
#'
#' In stem- and ViewFull-tables `status` and `Status` refer to each stem. In
#' tree-tables `status` refers to tree. To get the status of each tree based
#' on the status of its stems see [add_status_tree()].
#'
#' @template x_fgeo
#' @param wood Either "stem" or "tree", to indicate if the data should be
#'   filtered based on the status of each individual stem or tree (notice that
#'   one tree is dead only when not some but all its stems are dead).
#' @param .status Character vector; Must be one of possible values of the
#'   variable giving the status of the dataframe `x`.
#' @param exclude Logical; `TRUE` filters the data to exclude the values passed
#'   to `.status`.
#'
#' @seealso [add_status_tree()], [fgeo.base::drop_status()].
#'
#' @family functions to pick or drop rows of a dataframe.
#'
#' @return A filtered version of the dataframe `x`.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(fgeo.tool)
#' 
#' #' # Filter by the status of each stem (wood = "stem") -----------------------
#' 
#' # CENSUS TABLE: STEM TABLE
#' 
#' x <- fgeo.data::luquillo_stem_random_tiny
#' table(x$status)
#' 
#' result <- filter_status(x, wood = "stem", .status = "D")
#' table(result$status)
#' 
#' result <- filter_status(x, wood = "stem", .status = "D", exclude = TRUE)
#' table(result$status)
#' # Shortcut
#' result <- drop_dead_stem(x)
#' table(result$status)
#' 
#' # Warns
#' result <- filter_status(x, wood = "stem", .status = c("A", "wrong-status"))
#' table(result$status)
#' 
#' # CENSUS TABLE: TREE TABLE
#' 
#' # Works exactly in the same way
#' x <- fgeo.data::luquillo_tree6_random
#' table(x$status)
#' 
#' result <- filter_status(x, wood = "stem", .status = "D")
#' table(result$status)
#' 
#' # Shortcut
#' result <- drop_dead_stem(x)
#' table(result$status)
#' 
#' # VIEWFULL TABLE
#' 
#' # Works exactly in the same way, but notice the following:
#' # * The variable Status starts with capital S;
#' # * The values of Status are not, say "A" or "D", but "alive" or "dead".
#' x <- fgeo.data::luquillo_vft_4quad
#' table(x$Status)
#' 
#' result <- filter_status(x, wood = "stem", .status = "alive")
#' table(result$Status)
#' 
#' # Warns because `.status` defaults to "D" -- not to "dead".
#' result <- drop_dead_stem(x)
#' # Fix and repeat
#' result <- drop_dead_stem(x, .status = "dead")
#' table(result$Status)
#' 
#' 
#' 
#' # Filter by the status of each tree (wood = "tree") -----------------------
#' 
#' # CENSUS TABLE: STEM TABLE
#' 
#' x <- fgeo.data::luquillo_stem_random_tiny
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
#' result <- drop_dead_tree(x)
#' result %>%
#'   dplyr::arrange(treeID, stemID, status) %>%
#'   dplyr::select(treeID, stemID, status, status_tree)
#' table(result$status_tree)
#' 
#' 
#' 
#' # CENSUS TABLE: TREE TABLE
#' x <- fgeo.data::luquillo_tree6_random
#' # For a tree census-table, each stem maps to a tree, so the value of the
#' # variable `status` gives, at the same time, the status of the stem and tree.
#' x <- add_status_tree(x, "D", "A")
#' identical(x$status, x$status_tree)
#' # So the result will be the same if we use `wood = tree` or `wood = stem`.
#' result1 <- filter_status(x, wood = "tree", .status = "A")
#' result2 <- filter_status(x, wood = "stem", .status = "A")
#' identical(result1, result2)
#' # Shortcut
#' result <- drop_dead_tree(x, .status = "D")
#' identical(result, result1)
#' 
#' 
#' 
#' # VIEWFULL TABLE
#' 
#' x <- fgeo.data::luquillo_vft_4quad
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
#' result <- drop_dead_tree(x, "dead")
#' result %>%
#'   dplyr::arrange(TreeID, StemID, Status) %>%
#'   dplyr::select(TreeID, StemID, Status, status_tree)
#' table(result$status_tree)
filter_status <- function(x, wood, .status, exclude = FALSE) {
  old_nms <- names(x)
  x <- set_names(x, tolower)
  check_filter_status(x = x, wood = wood, .status = .status, exclude = exclude)

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

  set_names(filtered, old_nms)
}

#' @rdname filter_status
#' @export
drop_dead_stem <- function(x, .status = "D") {
  filter_status(x = x, wood = "stem", .status = .status, exclude = TRUE)
}

#' @rdname filter_status
#' @export
drop_dead_tree <- function(x, .status = "D") {
  filter_status(x = x, wood = "tree", .status = .status, exclude = TRUE)
}

check_filter_status <- function(x, wood, .status, exclude) {
  stopifnot(is.data.frame(x))
  stopifnot(is.character(.status))
  stopifnot(is.character(wood))
  stopifnot(length(wood) == 1)
  stopifnot(wood  %in% c("stem", "tree"))
  if (wood == "stem") {
    check_valid_status(x, .status, "status")
    na_n <- sum(is.na(x$status))
    if (na_n > 0) {warning("Ignoring ", na_n, " NA(s)", call. = FALSE)}
  }
  if (wood == "tree") {
    check_valid_status(x, .status, "status_tree")
    na_n <- sum(is.na(x$status_tree))
    if (na_n > 0) {warning("Ignoring ", na_n, " NA(s)", call. = FALSE)}
  }
  stopifnot(is.logical(exclude))
}

check_valid_status <- function(x, .status, status_var) {
  .status_var <- x[[status_var]]
  check_crucial_names(x, status_var)
  valid_status <- unique(.status_var)
  invalid_status <- setdiff(.status, valid_status)
  if (length(invalid_status) != 0) {
    warning(
      "No observation has .status = ", commas(invalid_status), "\n",
      "  * Detected values: ", commas(valid_status),
      call. = FALSE
    )
  }
}
