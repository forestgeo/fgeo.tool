input <- tool_example("example.xlsx")

context("xlsheets_list")

x <- xlsheets_list(input)

test_that("input is a list of data frames", {
  expect_type(x, "list")
  expect_true(each_list_item_is_df(x))
})



context("files_to_df")

library(fs)

test_that("outputs expected object", {
  dir <- tool_example("files")

  expect_is(csv_df(dir), "data.frame")
  expect_is(csv_list(dir), "list")

  expect_is(xl_df(dir), "data.frame")
  expect_is(xl_list(dir), "list")
})
