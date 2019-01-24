context("from_var_to_var")

test_that("gxgy_to_qxqy() otputs the expected names", {
  out <- suppressMessages(gxgy_to_qxqy(c(1, 1), c(1, 1), gridsize = 20))
  expect_named(out, c("QX", "QY"))
})

test_that("handles normal cases", {
  expect_error(gxgy_to_hectindex(0, 0, c(20, 20)), NA)

  expect_error(gxgy_to_hectindex(c(0, NA), c(0, 0), c(20, 20)), NA)
  expect_error(gxgy_to_hectindex(NA, 0, c(20, 20)), NA)

  expect_warning(gxgy_to_hectindex(20, 0, c(20, 20)), "at.*beyond.*limits")
  expect_warning(gxgy_to_hectindex(0, 20, c(20, 20)), "at.*beyond.*limits")
  expect_warning(gxgy_to_hectindex(20, 20, c(20, 20)), "at.*beyond.*limits")
})

gridsize <- 20
plotdim <- c(1000, 500)

test_that("operations are reversible", {
  input <- 1:3
  output <- gxgy_to_index(
    index_to_gxgy(input, gridsize, plotdim)$gx,
    index_to_gxgy(input, gridsize, plotdim)$gy,
    gridsize, plotdim
  )
  expect_equal(input, output)
})

test_that("guesses plotdim if plotdim is missing", {
  expect_message(gxgy_to_index(0, 0, gridsize = 20), "Guessing")
  expect_message(gxgy_to_rowcol(0, 0, gridsize = 20), "Guessing")
  expect_warning(
    expect_message(gxgy_to_hectindex(0, 0), "Guessing"),
    "at.*beyond.*limits"
  )
})
