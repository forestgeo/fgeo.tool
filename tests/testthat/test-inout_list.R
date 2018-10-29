context("dir_list")

library(fs)

test_that("outputs expected object", {
  dir <- tool_example("csv")
  expect_is(csv_list(dir), "list")
  
  dir <- tool_example("xl")
  expect_is(xl_list(dir), "list")
})
