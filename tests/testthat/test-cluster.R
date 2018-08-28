context("cluster")

describe("cluster", {
  elev_ls <- fgeo.data::luquillo_elevation
  gridsize <- 20
  n <- 4
  topo <- fgeo_topography(elev_ls, gridsize = gridsize)
  clustered <- cluster(topo, n = n)
  
  it("has the expected structure", {
    nms <- c("gx", "gy", "meanelev", "convex", "slope", "cluster")
    expect_named(clustered, nms)
    expect_is(clustered, "fgeo_topography")
    
    n_plot_row <- elev_ls$xdim / gridsize
    n_plot_col <- elev_ls$ydim / gridsize
    n_quadrats <- n_plot_row * n_plot_col
    expect_equal(nrow(clustered), n_quadrats)
  })
  
  it("errs with informative error messages", {
    expect_error(cluster(tibble(x = 1)), "Can't deal with data of class")
    expect_error(cluster(1), "Can't deal with data of class")
    expect_error(cluster(topo, "a"), "`n` must be numeric")
    
  })
})
