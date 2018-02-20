context("type_fgeo.R")

test_that("outputs a list", {
  expect_type(type_vft(), "list")
  expect_length(type_vft(), 32)
  expect_type(type_taxa(), "list")
  expect_length(type_taxa(), 21)
})
