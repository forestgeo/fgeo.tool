context("fgeo_habitat.R")

describe("fgeo_habitat", {
  skip_if_not_installed("fgeo.habitat")
  library(fgeo.habitat)
  
  census <- luquillo_top3_sp
  census <- census[census$status == "A" & census$dbh >= 10, ]
  species <- c("CASARB", "PREMON", "SLOBER")
  
  it("outputs object that throws no warning with tt_test()", {
    elev_ls <- fgeo.data::luquillo_elevation
    habitat_ls <- fgeo_habitat(elev_ls, gridsize = 20, n = 4)
    expect_silent(expect_message(tt_test(census, species, habitat_ls)))
    
    elev_df <- fgeo.data::luquillo_elevation$col
    habitat_df <- fgeo_habitat(
      elev_df, gridsize = 20, n = 4, xdim = elev_ls$xdim, ydim = elev_ls$ydim
    )
    expect_silent(expect_message(tt_test(census, species, habitat_df)))
    
    # FIXME
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
})

test_that("outputs object with number of rows equal to number of quadrats", {
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

test_that("it works with data from bci", {
  skip_if_not_installed("bciex")
  elev <- bciex::bci_elevation
  expect_error(
    fgeo_habitat(elev, gridsize = 20, 2),
    "`xdim` and `ydim` can't be missing if `elevation` is a data.frame"
  )
  expect_silent(fgeo_habitat(elev, gridsize = 20, 2, xdim = 1000, ydim = 500))
  bci_elev_ls <- list(col = elev, xdim = 1000, ydim = 500)
  expect_silent(fgeo_habitat(bci_elev_ls, gridsize = 20, n = 4))
})

test_that("errs with informative messages", {
  expect_error(fgeo_habitat(1), "Can't deal with data of class")
  expect_error(fgeo_habitat(fgeo.data::luquillo_elevation), "is missing")
  expect_error(
    fgeo_habitat(fgeo.data::luquillo_elevation$col), 
    "can't be missing"
  )
  
  elev_missing_xydims <- list(col = fgeo.data::luquillo_elevation$col)
  expect_error(fgeo_habitat(elev_missing_xydims), "Ensure your data set")
  expect_error(fgeo_habitat(fgeo.data::luquillo_elevation, 20), "is missing")
})



context("measure_topography")

describe("measure_topography", {
  elev_ls <- fgeo.data::luquillo_elevation
  it("Outputs the expected data structure", {
    gridsize <- 20
    out <- measure_topography(elev_ls, gridsize = gridsize, n = 4)
    expect_is(out, "tbl_df")
    expect_named(out, c("gx", "gy", "meanelev", "convex", "slope"))
    
    n_plot_row <- elev_ls$xdim / gridsize
    n_plot_col <- elev_ls$ydim / gridsize
    n_quadrats <- n_plot_row * n_plot_col
    expect_equal(nrow(out), n_quadrats)
  })
})



context("cluster_elevation")

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
})
