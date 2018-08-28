describe("fgeo_topography", {
  it("Outputs the expected data structure", {
    elev_ls <- fgeo.data::luquillo_elevation
    gridsize <- 20
    out <- fgeo_topography(elev_ls, gridsize = gridsize)
    expect_is(out, "tbl_df")
    expect_named(out, c("gx", "gy", "meanelev", "convex", "slope"))
    
    n_plot_row <- elev_ls$xdim / gridsize
    n_plot_col <- elev_ls$ydim / gridsize
    n_quadrats <- n_plot_row * n_plot_col
    expect_equal(nrow(out), n_quadrats)
  })
  
  it("Works with both elevation list and dataframe", {
    elev_ls <- fgeo.data::luquillo_elevation
    gridsize <- 20
    expect_silent(out_ls <- fgeo_topography(elev_ls, gridsize))
    expect_silent(
      out_df <- fgeo_topography(
        elev_ls$col, gridsize, xdim = elev_ls$xdim, ydim = elev_ls$ydim)
    )
    expect_identical(out_ls, out_df)
  })
  
  it("errs with informative message", {
    expect_error(fgeo_topography(1), "Can't deal with.*numeric")
    
    elev_ls <- fgeo.data::luquillo_elevation
    expect_error(fgeo_topography(elev_ls), "gridsize.*is missing")
    expect_error(fgeo_topography(elev_ls$col), "gridsize.*is missing")
    expect_error(fgeo_topography(elev_ls$col, 20), "xdim.*can't be `NULL`")
  })
})



describe("cluster_elevation", {
  it("outputs equal to a known ", {
    elev_ls <- fgeo.data::luquillo_elevation
    gridsize <- 20
    out <- cluster_elevation(elev_ls, gridsize = gridsize, n = 4)
    expect_known_output(out, "ref-cluster_elevation", print = TRUE)
  })
  
  it("Outputs the expected data structure", {
    elev_ls <- fgeo.data::luquillo_elevation
    gridsize <- 20
    out <- cluster_elevation(elev_ls, gridsize = gridsize, n = 4)
    expect_is(out, "tbl_df")
    expect_named(out, c("gx", "gy", "meanelev", "convex", "slope", "cluster"))
    
    n_plot_row <- elev_ls$xdim / gridsize
    n_plot_col <- elev_ls$ydim / gridsize
    n_quadrats <- n_plot_row * n_plot_col
    expect_equal(nrow(out), n_quadrats)
  })
  
  it("Works with both elevation list and dataframe", {
    elev_ls <- fgeo.data::luquillo_elevation
    gridsize <- 20
    expect_silent(out_ls <- cluster_elevation(elev_ls, gridsize, n = 4))
    expect_silent(
      out_df <- cluster_elevation(
        elev_ls$col, gridsize, n = 4, xdim = elev_ls$xdim, ydim = elev_ls$ydim
      )
    )
    expect_identical(out_ls, out_df)
  })
  
  it("errs with informative message", {
    expect_error(cluster_elevation(1), "Can't deal with.*numeric")
    
    elev_ls <- fgeo.data::luquillo_elevation
    expect_error(cluster_elevation(elev_ls, n = 4), "gridsize.*is missing")
    expect_error(cluster_elevation(elev_ls$col, n = 4), "gridsize.*is missing")
    
    expect_error(cluster_elevation(elev_ls, 20), "n.*is missing")
    expect_error(cluster_elevation(elev_ls$col, 20), "n.*is missing")
    expect_error(
      cluster_elevation(elev_ls$col, gridsize = 20, n = 4),
      "xdim.*can't be `NULL`"
    )
  })
  
  it("is sensitive to `edgecorrect`", {
    elev <- fgeo.data::luquillo_elevation
    out1 <- cluster_elevation(elev, 20, 4, edgecorrect = FALSE)
    out2 <- cluster_elevation(elev, 20, 4, edgecorrect = TRUE)
    expect_false(identical(out1, out2))
  })
})
