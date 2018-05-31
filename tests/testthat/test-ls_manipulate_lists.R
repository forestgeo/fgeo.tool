input <- system.file("extdata", "example.xlsx", package = "fgeo.tool")



context("ls_list_spreadsheets")

x <- ls_list_spreadsheets(input)

test_that("input is a list of data frames", {
  expect_type(x, "list")
  expect_true(each_list_item_is_df(x))
})



context("ls_csv_df")

df_list <- ls_list_spreadsheets(input)
output <- tempdir()
test_that("errs with wrong input", {
  expect_error(ls_csv_df(1, output))
  expect_error(ls_csv_df(list(1), output))
  expect_error(ls_csv_df(df_list, 1))
  expect_error(ls_csv_df(df_list, output, prefix = 1))
})

test_that("works as expected", {
  ls_csv_df(df_list, output, prefix = "myfile-")
  files <- dir(output)
  expect_true(length(files[grepl("^myfile.*csv$", files)]) > 0)
})



context("ls_join_df")

df_list <- list(
  a = data.frame(x = 1),
  b = data.frame(x = 2, y = 2),
  c = data.frame(x = 1, z = 3)
)

test_that("errs with wrong input", {
  expect_error(ls_join_df(1))
  expect_error(ls_join_df(data.frame(1)))
  expect_error(ls_join_df(df_list, 1))
})

test_that("works as expected", {
  x <- ls_join_df(df_list, df_names = c("a", "c"))
  expect_equal(names(x), c("x", "z"))
  x <- ls_join_df(df_list, df_names = c("b", "c"))
  expect_equal(names(x), c("x", "y", "z"))
  expect_silent(ls_join_df(list(data.frame(1))))
  expect_silent(
    ls_join_df(
      list(data.frame(x = 1), data.frame(z = 2)),
      by = c("x" = "z")
    )
  )
})

