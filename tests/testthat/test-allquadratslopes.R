context("allquadratslopes")

elev <- fgeo.x::elevation

test_that("works with simple input", {
  # The input to elev is very specific; you may need to tweak it.
  result <- allquadratslopes(
    elev = elev,
    gridsize = 20,
    plotdim = c(1000, 500),
    edgecorrect = TRUE
  )

  expect_is(result, "data.frame")
  expect_named(result, c("meanelev", "convex", "slope"))
})

test_that("warns if input to eleve is unexpected", {
  names(elev)[[1]] <- "data"
  expect_error(
    expect_warning(
      allquadratslopes(
        elev = elev,
        gridsize = 20,
        plotdim = c(1000, 500),
        edgecorrect = TRUE
      ),
      "must.*named.*col"
    )
  )
})



context("calcslope")

test_that("errs with informative message", {
  gridsize <- 20
  # Passes with good input
  expect_error(calcslope(1:3, gridsize), NA)
  expect_error(calcslope(1:2, gridsize), "must be of length 3")
  expect_error(calcslope(1, gridsize), "must be of length 3")
  expect_error(calcslope(1:4, gridsize), "must be of length 3")
})

test_that("with z == 0 returns 0", {
  expect_equal(calcslope(c(0, 0, 0), 20), 0)
  expect_equal(calcslope(c(1, 1, 1), 20), 0)
})



context("warn_if_no_data_falls_on_half_gridsize")

test_that("does what it promises", {
  col <- tibble::tribble(
     ~x,  ~y, ~elev,
      0,   0,     0,
     15,  15,     0,
  )
  expect_warning(
    warn_if_no_data_falls_on_half_gridsize(
      list(col = col), gridsize = 20, edgecorrect = TRUE
    ),
    "elevation.*too coarse"
  )
})
