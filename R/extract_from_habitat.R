#' Extract plot dimensions from habitat data.
#'
#' @param habitats Data frame giving the habitat designation for each 20x20
#'   quadrat.
#' @name extract_from_habitat
#'
#' @return
#' * [extract_plotdim()]: `plotdim` (vector of length 2);
#' * [extract_gridsize()]: `gridsize` (scalar).
#'
#' @examples
#' extract_plotdim(luquillo_habitat)
#' extract_gridsize(luquillo_habitat)
NULL

#' @rdname extract_from_habitat
#' @export
extract_gridsize <- function(habitats) {
  stopifnot(is.data.frame(habitats))
  habitats <- tryCatch(
    fgeo.base::check_crucial_names(habitats, c("x", "y")), 
    error = function(e) rename_to_xy(habitats)
  )
  fgeo.base::warn_na(habitats)
  fgeo.base::check_crucial_names(habitats, c("x", "y"))

  grid_x <- difference_among_grid_steps(habitats$x)
  grid_y <- difference_among_grid_steps(habitats$y)
  gridsize <- unique(grid_x, grid_y)
  as.integer(gridsize)
}

#' @rdname extract_from_habitat
#' @export
extract_plotdim <- function(habitats) {
  habitats <- tryCatch(
    fgeo.base::check_crucial_names(habitats, c("x", "y")), 
    error = function(e) rename_to_xy(habitats)
  )
  
  gridsize <- extract_gridsize(habitats)
  plotdim <- unlist(
    lapply(habitats[c("x", "y")], function(.x) {
      max(.x) + gridsize
    })
  )
  as.integer(unname(plotdim))
}

rename_to_xy <- function(x) {
  .x <- x
  .x <- fgeo.tool::nms_try_rename(.x, want = "x", try = "gx")
  .x <- fgeo.tool::nms_try_rename(.x, want = "y", try = "gy")
  .x
}



#' From x and y columns of habitat data, get difference between grid steps.
#'
#' @param habitat_x_or_y Column x or y of habitat data, e.g. luquillo_habitat$x.
#'
#' @return A non negative scalar
#' @noRd
difference_among_grid_steps <- function(habitat_x_or_y) {
  grid_steps <- unique(habitat_x_or_y)
  difference_among_grid_steps <- unique(diff(grid_steps))

  difference_among_grid_steps
}
