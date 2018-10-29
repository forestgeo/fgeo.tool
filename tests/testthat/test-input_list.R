context("input_list")

library(fs)

test_that("outputs expected object", {
  dir <- tool_example("csv")
  expect_is(csv_list(dir), "list")
  
  dir <- tool_example("xl")
  expect_is(xl_list(dir), "list")

  dir <- tool_example("rds")
  expect_is(rds_list(dir), "list")
  
  dir <- tool_example("rdata")
  expect_is(rdata_list(dir), "list")
})

test_that("can specific files in a mixed directory", {
  dir <- tool_example("mixed_files")
  expect_is(csv_list(dir), "list")
})
