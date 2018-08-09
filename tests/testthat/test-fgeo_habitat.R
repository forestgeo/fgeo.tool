context("fgeo_habitat")

describe("fgeo_habitat", {
  skip_if_not_installed("fgeo.habitat")
  library(fgeo.habitat)
  
  census <- luquillo_top3_sp
  census <- census[census$status == "A" & census$dbh >= 10, ]
  species <- c("CASARB", "PREMON", "SLOBER")
  
  it("errs with informative messages", {
    expect_error(fgeo_habitat(1), "Can't deal with.*numeric")
    
    elev_ls <- fgeo.data::luquillo_elevation
    expect_error(fgeo_habitat(elev_ls), "gridsize.*is missing")
    expect_error(fgeo_habitat(elev_ls, 20), "n.*is missing")
    
    elev_ls_missing_xdim <- elev_ls
    elev_ls_missing_xdim$xdim <- NULL
    expect_error(fgeo_habitat(elev_ls_missing_xdim), "Ensure your data set")
    
    expect_error(fgeo_habitat(elev_ls$col), "xdim.*ydim.*can't be `NULL`")
  })
  
  it("outputs object that throws no warning with tt_test()", {
    elev_ls <- fgeo.data::luquillo_elevation
    habitat_ls <- fgeo_habitat(elev_ls, gridsize = 20, n = 4)
    expect_silent(expect_message(tt_test(census, species, habitat_ls)))
    
    elev_df <- fgeo.data::luquillo_elevation$col
    habitat_df <- fgeo_habitat(
      elev_df, gridsize = 20, n = 4, xdim = elev_ls$xdim, ydim = elev_ls$ydim
    )
    expect_silent(expect_message(tt_test(census, species, habitat_df)))
    expect_identical(habitat_df, habitat_ls)
  })

  it("plots with plot.fgeo_habitat()", {
    skip_if_not_installed("fgeo.map")
    library(fgeo.map)
    
    elev_ls <- fgeo.data::luquillo_elevation
    habitat <- fgeo_habitat(elev_ls, gridsize = 20, n = 4)
    p <- plot(habitat)
    expect_is(p, "ggplot")
  })
  
  it("results in gx and gy that are multiple of gridsize", {
    elev_ls <- fgeo.data::luquillo_elevation
    gridsize <- 20
    habitat <- fgeo_habitat(elev_ls, gridsize = gridsize, n = 4)
    expect_true(all(habitat$gx %% gridsize == 0))
  })
  
  it("outputs object with number of rows equal to number of quadrats", {
    elev_luq <- fgeo.data::luquillo_elevation
    hab <- fgeo_habitat(elev_luq, gridsize = 20, n = 4)
    
    plotdim <- c(320, 500)
    gridsize <- 20
    rows <- plotdim[[1]] / gridsize
    cols <- plotdim[[2]] / gridsize
    expect_equal(nrow(hab), rows * cols)
    
    # Reference: This habitat dataset was created by the authors of tt_test()
    habitat_pasoh <- pasoh::pasoh_hab_index20
    pd <- c(1000, 500)
    gs <- 20
    rw <- pd[[1]] / gs
    cl <- pd[[2]] / gs
    expect_equal(nrow(habitat_pasoh), rw * cl)
  })
  
  it("works with data from bci", {
    skip_if_not_installed("bciex")
    elev <- bciex::bci_elevation
    expect_error(
      fgeo_habitat(elev, gridsize = 20, n = 2), "xdim.*ydim.*can't be `NULL`"
    )
    expect_silent(fgeo_habitat(elev, gridsize = 20, 2, xdim = 1000, ydim = 500))
    bci_elev_ls <- list(col = elev, xdim = 1000, ydim = 500)
    expect_silent(fgeo_habitat(bci_elev_ls, gridsize = 20, n = 4))
  })
  
  it("is sensitive to `only_elev`", {
    elev <- fgeo.data::luquillo_elevation
    out1 <- fgeo_habitat(elev, 20, 4, only_elev = FALSE)
    out2 <- fgeo_habitat(elev, 20, 4, only_elev = TRUE)
    expect_false(identical(out1, out2))
  })
  
  it("is sensitive to `edgecorrect`", {
    elev <- fgeo.data::luquillo_elevation
    out1 <- fgeo_habitat(elev, 20, 4, edgecorrect = FALSE)
    out2 <- fgeo_habitat(elev, 20, 4, edgecorrect = TRUE)
    expect_false(identical(out1, out2))
  })
})



describe("measure_topography", {
  it("Outputs the expected data structure", {
    elev_ls <- fgeo.data::luquillo_elevation
    gridsize <- 20
    out <- measure_topography(elev_ls, gridsize = gridsize)
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
    expect_silent(out_ls <- measure_topography(elev_ls, gridsize))
    expect_silent(
      out_df <- measure_topography(
        elev_ls$col, gridsize, xdim = elev_ls$xdim, ydim = elev_ls$ydim)
    )
    expect_identical(out_ls, out_df)
  })
  
  it("errs with informative message", {
    expect_error(measure_topography(1), "Can't deal with.*numeric")
    
    elev_ls <- fgeo.data::luquillo_elevation
    expect_error(measure_topography(elev_ls), "gridsize.*is missing")
    expect_error(measure_topography(elev_ls$col), "gridsize.*is missing")
    expect_error(measure_topography(elev_ls$col, 20), "xdim.*can't be `NULL`")
  })
})



describe("cluster_elevation", {
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
