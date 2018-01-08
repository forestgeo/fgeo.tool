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

#' @rdname status_stem
#' @export
stem_not_dead <- function(x, .status = "D") {
  filter_status(x = x, wood = "stem", .status = .status, exclude = TRUE)
}

#' @rdname status_stem
#' @export
tree_not_dead <- function(x, .status = "dead") {
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
      "  * Available options are: ", collapse(valid_status)
    )
  }
}
