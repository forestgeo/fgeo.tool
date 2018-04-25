#' Create habitat data from elevation data.
#'
#' @param elevation List containing elevation data.
#' @param gridsize Number to round `x` and `y` by.
#' @param n Number of elevation groups (habitats) to cut elevation data by.
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
      habitats = as.integer(ggplot2::cut_number(elev, n)),
      elev = NULL
    ) %>% 
    dplyr::filter(x < elevation$xdim, y < elevation$ydim)
}

