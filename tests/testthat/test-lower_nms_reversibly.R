context("names_lowercase.R")


test_that("errs with wrong input", {
  expect_error(
    names_lowercase(1)
  )
  expect_error(
    names_restore(data.frame(a = 1))
  )
})

test_that("returns as expected", {
  cns <- tibble::tibble(CensusID = 1, status = "A")
  original <- cns
  lowered <- names_lowercase(cns)

  expect_true(
    !is.null(
      attr(lowered, "names_old")
    )
  )

  expect_false(identical(original, lowered))

  expect_identical(
    names(rlang::set_names(original, tolower)),
    names(lowered)
  )
})

context("names_restore")

test_that("reverses the effect of names_lowercase()", {
  cns <- tibble::tibble(CensusID = 1, status = "A")
  original <- cns %>% names()
  forth_and_back <- cns %>% names_lowercase() %>% names_restore()
  expect_identical(original, names(forth_and_back))
})
