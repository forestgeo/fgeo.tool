context("add_id")

test_that("outputs the expected data structure", {
  .data <- tibble::tibble(x = 1:2, y = c("a", "b"), z = c(TRUE, FALSE))
  vars <- c("x", "y", "z")
  name <- "combo"
  sep <- "_"
  out <- add_id(.data, vars, name, sep)
  expect_is(out, "data.frame")
  expect_named(out, c("x", "y", "z", "combo"))
  expect_identical(out$combo[[1]], "1_a_TRUE")
})

test_that("fails with expected message", {
  expect_error(add_id(1), "is not TRUE")
})
