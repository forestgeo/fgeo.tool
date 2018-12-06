context("drop_if_na")

test_that("fails as expected", {
  expect_error(drop_if_na(1), "must be a dataframe.")
  expect_error(drop_if_na(data.frame(1)), "is missing")
  expect_error(drop_if_na(data.frame(a = 1), x = "not_nm"), "must be a column")
  expect_error(
    drop_if_na(data.frame(a = 1), x = c("too", "long")), "must be of length 1"
  )
})

test_that("outputs expected warning and structure", {
  dfm <- data.frame(x = c(NA), y = 1)
  expect_warning(out <- drop_if_na(dfm, "x"), "Dropping 1 rows")
  expect_is(out, "data.frame")
  expect_length(out, 2)
  expect_equal(nrow(out), 0)
})
