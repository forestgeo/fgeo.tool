context("extract_from_habitat")

test_that("with habitats = fgeo.x::habitat gridsize value is 20", {
  expect_equal(extract_gridsize(fgeo.x::habitat), 20)
  expect_equal(extract_gridsize(fgeo.x::habitat), 20)
})

test_that("with habitats = fgeo.x::habitat plotdim value is c(320,  500)", {
  expect_equal(extract_plotdim(fgeo.x::habitat)[[1]], 320)
  expect_equal(extract_plotdim(fgeo.x::habitat)[[2]], 500)
})

test_that("value are of correct type", {
  expect_type(extract_gridsize(fgeo.x::habitat), "integer")
  expect_type(extract_plotdim(fgeo.x::habitat), "integer")
})

test_that("Output of extract_plotdim is unnamed", {
  expect_null(names(extract_plotdim(fgeo.x::habitat)))
})

test_that("output is of correct lengh", {
  expect_length(extract_gridsize(fgeo.x::habitat), 1)
  expect_length(extract_plotdim(fgeo.x::habitat), 2)
})

test_that("fails with wrong names", {
  expect_error(
    extract_gridsize(data.frame(wrong_name = 1)),
    "Data must have"
  )
  expect_error(
    extract_plotdim(data.frame(wrong_name = 1)),
    "Data must have"
  )
})

test_that("passes with names either x,y or gx,gy", {
  gxgy <- fgeo.x::habitat
  xy <- setNames(fgeo.x::habitat, c("x", "y", "habitats"))
  expect_error(out_gxgy <- extract_plotdim(gxgy), NA)
  expect_error(out_xy <- extract_plotdim(xy), NA)
  expect_identical(out_gxgy, out_xy)
})

test_that("warns if habitat data contains NA", {
  dfm <- dplyr::tibble(
    x = c(0, 999, NA),
    y = c(0, 499, NA),
    habitats = c(1, 2, 3)
  )
  expect_silent(extract_plotdim(dfm[1:2, ]))
  expect_warning(extract_plotdim(dfm), "Detected missing values in: x, y")
  expect_silent(extract_gridsize(dfm[1:2, ]))
  expect_warning(extract_gridsize(dfm), "Detected missing values in: x, y")
})
