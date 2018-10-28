context("print-tbl")

test_that("prints all rows", {
  skip_if_not_installed("tibble")
  tbl <- tibble::tibble(x = as.character(runif(21)))
  tbl[21, "x"] <- "row21"
  expect_output(print(tbl, n = nrow(tbl)), "row21")
})
