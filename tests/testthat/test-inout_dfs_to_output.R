input <- tool_example("example.xlsx")

context("list_csv")

lst <- xlsheets_list(input)
output <- tempdir()
test_that("errs with wrong input", {
  expect_error(list_csv(1, output))
  expect_error(list_csv(list(1), output))
  expect_error(list_csv(lst, 1))
  expect_error(list_csv(lst, output, prefix = 1))
})

test_that("works as expected", {
  list_csv(lst, output, prefix = "myfile-")
  files <- dir(output)
  expect_true(length(files[grepl("^myfile.*csv$", files)]) > 0)
})



context("list_df")

lst <- list(
  a = data.frame(x = 1),
  b = data.frame(x = 2, y = 2),
  c = data.frame(x = 1, z = 3)
)

test_that("errs with wrong input", {
  expect_error(list_df(1))
  expect_error(list_df(data.frame(1)))
  expect_error(list_df(lst, 1))
})

test_that("works as expected", {
  x <- list_df(lst, df_names = c("a", "c"))
  expect_equal(names(x), c("x", "z"))
  x <- list_df(lst, df_names = c("b", "c"))
  expect_equal(names(x), c("x", "y", "z"))
  expect_silent(list_df(list(data.frame(1))))
  expect_silent(
    list_df(
      list(data.frame(x = 1), data.frame(z = 2)),
      by = c("x" = "z")
    )
  )
})

