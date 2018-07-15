context("utils")

test_that("multiple_censusid() works as epxected with any case", {
  expect_true(multiple_censusid(tibble(CensusID = c(1, 2, NA))))
  expect_true(multiple_censusid(tibble(censusid = c(1, 2, NA))))
})
