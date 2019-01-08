# ?ctfs::quad.to.gxgy()
head(ctfs::quad.to.gxgy(fgeo.x::vft_4quad$QuadratName))

#' @author Richard Condit
quad_to_gxgy <- function(x, gridsize = 20, start = 0) {
  x = as.numeric(x)
  rowno = x %% 100 - start
  colno = floor(x / 100) - start
  data.frame(gx = colno * gridsize, gy = rowno * gridsize)
}

add_gxgy <- function(x, gridsize = 20, start = 0) {
  fgeo.tool::check_crucial_names(x, "QuadratName")
  newcol <- quad_to_gxgy(x, gridsize = gridsize, start = start)
  tibble::as_tibble(cbind(x, newcol))
}

quad_to_gxgy(0)

quad_to_gxgy(0:3)
quad_to_gxgy("0")
quad_to_gxgy("01")
quad_to_gxgy("01.1")
quad_to_gxgy("1010")
quad_to_gxgy(NA)
quad_to_gxgy(NULL)
quad_to_gxgy()
