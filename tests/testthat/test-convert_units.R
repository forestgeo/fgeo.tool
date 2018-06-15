context("convert_units")

test_that("output has correct structure", {
  expect_is(convert_units(1, "mm", "m"), "numeric")
  
  dfm <- data.frame(a = 1, b = 1)
  expect_is(
    out <- convert_units_at(dfm, .at = "a", "mm", "m"), "data.frame"
  )
  expect_named(out, c("a", "b"))
  expect_equal(out[[1]], 1/1000)
  expect_equal(out[[2]], 1)
})

test_that("converts correctly", {
  expect_equal(convert_units(123, "km/h", "m/s"), 123 * 1000 / (60 * 60))
  expect_equal(convert_units(1, "hectare", "m^2"), 10000)
})
