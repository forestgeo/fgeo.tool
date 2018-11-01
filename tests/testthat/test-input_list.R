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

test_that("can read specific files in a mixed directory", {
  dir <- tool_example("mixed_files")
  expect_is(csv_list(dir), "list")
})



context("read_with")

test_that("read_with() can handle cero, and two .rdata files", {
  empty <- tool_example("empty")
  expect_warning(rdata_list(empty), "Can't find.*rdata")
  expect_warning(
    read_with(readr::read_csv, "[.]csv")(empty), 
    "Can't find.*csv"
  )
})

