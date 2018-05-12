context("files_to_df")

library(fs)

test_that("outputs expected object", {
  path <- system.file("extdata", "files/01.csv", package = "fgeo.tool")
  input_dir <- path_dir(path)
  input_dir
  dir(input_dir)

  expect_is(csv_to_df(input_dir), "data.frame")
  expect_is(csv_to_df_lst(input_dir), "list")

  expect_is(xl_to_df(input_dir), "data.frame")
  expect_is(xl_to_df_lst(input_dir), "list")
})
