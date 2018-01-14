context("nms_restore_newvar")

test_that("restore the expected names", {
  # Data does not contain the variable that will be added
  dfm <- data.frame(X = 1, Y = "a")
  old <- names(dfm)
  # Lower names
  dfm <- rlang::set_names(dfm, tolower)
  # Add a variable
  mutated <- mutate(dfm, newvar = x + 1)
  # Restore
  out <- nms_restore_newvar(mutated, "newvar", old)
  expect_equal(names(out), c(old, "newvar"))

  # Data contains the variable that will be added
  dfm <- data.frame(X = 1, Y = "a", newvar = "2")
  (old <- names(dfm))
  # Lower names
  (dfm <- rlang::set_names(dfm, tolower))
  # Add a variable
  mutated <- mutate(dfm, newvar = x + 1)
  # Restore
  out <- nms_restore_newvar(mutated, "newvar", old)
  expect_equal(names(out), old)
})

test_that("fails if the number of variables is wrong", {
  # Data does not contain the variable that will be added
  dfm <- data.frame(X = 1, Y = "a")
  old <- names(dfm)
  # Lower names
  dfm <- rlang::set_names(dfm, tolower)
  # Add a variable
  mutated <- mutate(dfm, newvar = x + 1)
  too_many <- mutated$too_many <- 1

  expect_error(nms_restore_newvar(too_many, "newvar", old))

  too_few <- mutated$x <- NULL
  expect_error(nms_restore_newvar(too_few, "newvar", old))
})

