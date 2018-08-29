library(fgeo.tool)

context("fgeo_habitat2")

describe("ouputs the same as fgeo_habitat", {
  skip_if_not_installed("fgeo.habitat")
  library(fgeo.habitat)
  
  elev_ls <- fgeo.data::luquillo_elevation
  out <- fgeo_habitat(elev_ls, gridsize = 20, n = 4)
  out2 <- fgeo_habitat2(elev_ls, gridsize = 20, n = 4)
  expect_equal(out2, out)
  
  elev_df <- fgeo.data::luquillo_elevation$col
  out <- fgeo_habitat(
    elev_df, gridsize = 20, n = 4, xdim = elev_ls$xdim, ydim = elev_ls$ydim
  )
  out2 <- fgeo_habitat2(
    elev_df, gridsize = 20, n = 4, xdim = elev_ls$xdim, ydim = elev_ls$ydim
  )
  expect_equal(out2, out)
})  



context("fgeo_habitat")

describe("fgeo_habitat", {
  skip_if_not_installed("fgeo.habitat")
  library(fgeo.habitat)
  
  elev_ls <- fgeo.data::luquillo_elevation
  gridsize <- 20
  plotdim <- c(320, 500)
  
  it("errs with informative messages", {
    expect_error(fgeo_habitat2(1), "Can't deal with.*numeric")
    expect_error(fgeo_habitat2(elev_ls), "gridsize.*is missing")
    expect_error(fgeo_habitat2(elev_ls, 20), "n.*is missing")
    elev_ls_missing_xdim <- elev_ls
    elev_ls_missing_xdim$xdim <- NULL
    expect_error(fgeo_habitat2(elev_ls_missing_xdim), "gridsize.*is missing")
    expect_error(
      fgeo_habitat2(elev_ls$col, gridsize = 20), "xdim.*ydim.*can't be `NULL`"
    )
  })
  
  habitat <- fgeo_habitat2(elev_ls, gridsize = 20, n = 4)
  it("plots with plot.fgeo_habitat()", {
    skip_if_not_installed("fgeo.map")
    library(fgeo.map)

    p <- plot(habitat)
    expect_is(p, "ggplot")
  })
  it("results in gx and gy that are multiple of gridsize", {
    expect_true(all(habitat$gx %% gridsize == 0))
  })
  
  it("is sensitive to `edgecorrect`", {
    out1 <- fgeo_habitat2(elev_ls, gridsize = 20, n = 4, edgecorrect = FALSE)
    expect_false(identical(out1, habitat))
  })
  
  census <- luquillo_top3_sp
  census <- census[census$status == "A" & census$dbh >= 10, ]
  species <- c("CASARB", "PREMON", "SLOBER")
  
  it("outputs object that throws no warning with tt_test()", {
    expect_silent(expect_message(tt_test(census, species, habitat)))
  })
  
  it("outputs identical with elevation list or dataframe", {
    elev_df <- fgeo.data::luquillo_elevation$col
    habitat_df <- fgeo_habitat2(
      elev_df, gridsize = 20, n = 4, xdim = elev_ls$xdim, ydim = elev_ls$ydim
    )
    expect_identical(habitat_df, habitat)
    expect_silent(expect_message(tt_test(census, species, habitat_df)))
  })

  it("with data from pasoh, it outputs rows equal to number of quadrats", {
    skip_if_not_installed("pasoh")
    
    rows <- plotdim[[1]] / gridsize
    cols <- plotdim[[2]] / gridsize
    expect_equal(nrow(habitat), rows * cols)
    
    # Reference: This habitat dataset was created by the authors of tt_test()
    habitat_pasoh <- pasoh::pasoh_hab_index20
    pd <- c(1000, 500)
    gs <- 20
    rw <- pd[[1]] / gs
    cl <- pd[[2]] / gs
    expect_equal(nrow(habitat_pasoh), rw * cl)
  })
})
