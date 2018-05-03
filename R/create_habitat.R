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
#' create_habitat(fgeo.data::luquillo_elevation, gridsize = 20, n = 4)
create_habitat <- function(elevation, gridsize, n) {
  stopifnot(!missing(elevation), !missing(gridsize), !missing(n))
  check_crucial_names(elevation, c("col", "xdim", "ydim"))
  
  elevation$col %>%
    dplyr::as_tibble() %>% 
    dplyr::mutate(
      x = fgeo.base::round_any(.data$x, gridsize),
      y = fgeo.base::round_any(.data$y, gridsize)
    ) %>% 
    unique() %>% 
    dplyr::group_by(.data$x, .data$y) %>%
    dplyr::summarise(elev = mean(.data$elev)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      habitats = as.integer(cut_number(.data$elev, n)), elev = NULL
    ) %>% 
    dplyr::filter(.data$x < elevation$xdim, .data$y < elevation$ydim)
}
