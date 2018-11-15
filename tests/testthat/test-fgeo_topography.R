context("fgeo_topography")

describe("fgeo_topography", {
  elev_ls <- fgeo.x::elevation
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

describe("fgeo_topography()", {
  it("returns known output", {
    # Helper to keep code DRY and update references in only one place
    expect_known <- function(object, file, update = FALSE) {
      testthat::expect_known_output(object, file, update = update, print = TRUE)
    }
    
    elev_luq <- fgeo.x::elevation
    luq <- fgeo_topography(elev_luq, gridsize = 20)
    expect_known(head(luq), "ref-fgeo_topography_luq_head")
    expect_known(tail(luq), "ref-fgeo_topography_luq_tail")
    
    elev_bci <- bciex::bci_elevation
    bci <- fgeo_topography(elev_bci, gridsize = 20, xdim = 1000, ydim = 500)
    expect_known(head(bci), "ref-fgeo_topography_bci_head")
    expect_known(tail(bci), "ref-fgeo_topography_bci_tail")
  })
})

describe("fgeo_topography()", {
  it("`edgecorrect` works with elevaiton data finer than gridsize / 2 (#59)", {
    # Degrade elevation data to make it coarser
    degrade_elev <- function(elev_ls, gridsize) {
      elev_ls$col <- elev_ls$col %>% 
        dplyr::filter(x %% gridsize == 0) %>% 
        dplyr::filter(y %% gridsize == 0)
      elev_ls
    }
    
    luq_elev <- fgeo.x::elevation
    
    msg <- "No elevation data found at `gridsize / 2`"
    expect_warning(
      expect_error(fgeo_topography(degrade_elev(luq_elev, 20), gridsize = 20)),
      msg
    )
    
    no_warning <- NA
    expect_warning(
      fgeo_topography(degrade_elev(luq_elev, 10), gridsize = 20), no_warning
    )
    
    tian_tong_elev <- readr::read_csv(test_path("test-data-tian_tong_elev.csv"))
    expect_warning(
      expect_error(
        fgeo_topography(tian_tong_elev, gridsize = 20, xdim = 500, ydim = 400)
      ),
      msg
    )
  })
})
