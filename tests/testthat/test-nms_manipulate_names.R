context("nms_lowercase.R")

test_that("errs with wrong input", {
  expect_error(
    nms_lowercase(1)
  )
  expect_error(
    nms_restore(data.frame(a = 1))
  )
})

test_that("returns as expected", {
  cns <- tibble::tibble(CensusID = 1, status = "A")
  original <- cns
  lowered <- nms_lowercase(cns)

  expect_true(
    !is.null(
      attr(lowered, "names_old")
    )
  )

  expect_false(identical(original, lowered))

  expect_identical(
    names(set_names(original, tolower)),
    names(lowered)
  )
})



context("nms_restore")

test_that("reverses the effect of nms_lowercase()", {
  cns <- tibble::tibble(CensusID = 1, status = "A")
  original <- cns %>% names()
  forth_and_back <- cns %>% nms_lowercase() %>% nms_restore()
  expect_identical(original, names(forth_and_back))
})



context("nms_restore_newvar")

test_that("restore the expected names", {
  # Data does not contain the variable that will be added
  dfm <- data.frame(X = 1, Y = "a")
  old <- names(dfm)
  # Lower names
  dfm <- set_names(dfm, tolower)
  # Add a variable
  mutated <- mutate(dfm, newvar = x + 1)
  # Restore
  out <- nms_restore_newvar(mutated, "newvar", old)
  expect_equal(names(out), c(old, "newvar"))

  # Data contains the variable that will be added
  dfm <- data.frame(X = 1, Y = "a", newvar = "2")
  (old <- names(dfm))
  # Lower names
  (dfm <- set_names(dfm, tolower))
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
  dfm <- set_names(dfm, tolower)
  # Add a variable
  mutated <- mutate(dfm, newvar = x + 1)
  too_many <- mutated$too_many <- 1

  expect_error(nms_restore_newvar(too_many, "newvar", old))

  too_few <- mutated$x <- NULL
  expect_error(nms_restore_newvar(too_few, "newvar", old))
})



context("nms_tidy")

test_that("works for dataframes", {
  dfm <- data.frame(`Hi mE` = 1)
  out <- nms_tidy(dfm)
  expect_equal(names(out), "hi.me")
})

test_that("works for character string", {
  chr <- c("Hi  mE")
  out <- nms_tidy(chr)
  expect_equal(out, "hi__me")
})

test_that("works other named vectors", {
  v <- c(`hi mE` = 1)
  out <- nms_tidy(v)
  expect_equal(names(out), "hi_me")

  v <- c(`hi mE` = "one")
  out <- nms_tidy(v)
  expect_equal(names(out), "hi_me")
})



context("nms_try_rename")

test_that("renames correctly", {
  expect_equal(nms_try_rename(list(a = 1), "A", "a"), list(A = 1))
  expect_equal(nms_try_rename(c(a = 1), "A", "a"), c(A = 1))
  expect_equal(nms_try_rename(data.frame(a = 1), "A", "a"), data.frame(A = 1))
  expect_equal(nms_try_rename(c(a = 1, 1), "A", "a"), c(A = 1, 1))
})

test_that("fails with informative message", {
  expect_error(nms_try_rename(1, "A", "A"), "Data must have an element named")
})
