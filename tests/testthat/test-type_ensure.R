context("type_ensure")

test_that("works as expected", {
  dfm <- tibble::tibble(
    x = 1:3,
    y = as.character(1:3),
    z = letters[1:3]
  )

  expect_warning(
    x <- type_ensure(dfm, c("x", "y", "z"), "numeric"),
    "y, z should be numeric"
  )
  expect_true(all(purrr::map_lgl(x, is.numeric)))

  expect_warning(
    x <- type_ensure(dfm, c("x", "y", "z"), "character"),
    "x should be character"
  )
  expect_true(all(purrr::map_lgl(x, is.character)))
})
