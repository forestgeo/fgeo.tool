# xxx test
# xxx document
# xxx write a helper to convert from A and D to alive and dead
# xxx use wood = tree with census tables
# xxx use wood = stem with vft
# xxx remove stem_status
# xxx check why last example in tmp.R is wrong

#' x <- tibble::tibble(status = LETTERS[1:4])
#' filter_status(x, wood = "stem", .status = c("B", "C"))
#' filter_status(x, wood = "stem", .status = "D")
#' filter_status(x, wood = "stem", .status = "D", exclude = TRUE)
#' # Shortcut
#' stem_not_dead(x)
#'
#' x <- bciex::bci12vft_mini
#' result <- filter_status(x, wood = "tree", .status = "dead")
#' result[1:3, "status_tree"]
#'
#' result <- filter_status(x, wood = "tree", .status = "dead", exclude = TRUE)
#' result[1:3, "status_tree"]
#'
#' # Shortcut
#' result <- tree_not_dead(x)
#' result[1:3, "status_tree"]

filter_status <- function(x, wood, .status, exclude = FALSE) {
  # xxx here I need to add argumet to define the codes for "alive" and "dead"
  # or better, to guess it from "A" and "D" [inside add_status_tree()].
  if (wood == "tree") {x <- add_status_tree(x)}

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
    check_crucial_names(x, "status")
    is_valid_status <- unique(x$status)
    stopifnot(.status %in% is_valid_status)
  }
  if (wood == "tree") {
    # crucial names are checked by check_add_status_tree()
    is_valid_status_tree <- unique(x$status_tree)
    stopifnot(.status %in% is_valid_status_tree)
  }
}

