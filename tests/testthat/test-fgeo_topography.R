context("fgeo_topography")

describe("fgeo_topography", {
  elev_ls <- fgeo.data::luquillo_elevation
  gridsize <- 20
  topo <- fgeo_topography(elev_ls, gridsize = gridsize)
  
  it("Outputs the expected data structure", {
    expect_is(topo, "tbl_df")
    expect_is(topo, "fgeo_topography")
    expect_named(topo, c("gx", "gy", "meanelev", "convex", "slope"))
    
    n_plot_row <- elev_ls$xdim / gridsize
    n_plot_col <- elev_ls$ydim / gridsize
    n_quadrats <- n_plot_row * n_plot_col
    expect_equal(nrow(topo), n_quadrats)
  })
  
  it("Works with both elevation list and dataframe", {
    expect_silent(topo)
    expect_silent(
      out_df <- fgeo_topography(
        elev_ls$col, gridsize, xdim = elev_ls$xdim, ydim = elev_ls$ydim)
    )
    expect_identical(topo, out_df)
  })
  
  it("errs with informative message", {
    expect_error(fgeo_topography(1), "Can't deal with.*numeric")
    expect_error(fgeo_topography(elev_ls), "gridsize.*is missing")
    expect_error(fgeo_topography(elev_ls$col), "gridsize.*is missing")
    expect_error(fgeo_topography(elev_ls$col, gridsize), "xdim.*can't be `NULL`")
  })
  
  it("works with elevation list and dataframe with x and y, or gx and gy", {
    elev_ls2 <- elev_ls
    elev_ls2[[1]] <- setNames(elev_ls2[[1]], c("gx", "gy", "elev"))
    topo2 <- fgeo_topography(elev_ls2, gridsize = gridsize)
    
    expect_silent(topo2)
    expect_identical(topo2, topo)
    
    expect_silent(
      out_df <- fgeo_topography(
        elev_ls2$col, gridsize, xdim = elev_ls2$xdim, ydim = elev_ls2$ydim)
    )
    expect_identical(topo2, out_df)
  })
})
