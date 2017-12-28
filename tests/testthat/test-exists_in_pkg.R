context("exists_in_pkg")

test_that("is silent or throws error with existing or missing object", {
  expect_silent(exists_in_pkg("cars", "datasets"))
  detach("package:datasets")
  expect_error(exists_in_pkg("cars", "datasets"))
})
