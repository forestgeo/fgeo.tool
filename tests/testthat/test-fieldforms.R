context("fieldforms.R")

test_that("handles problematic input correctly", {
  expect_error(
    fieldforms_output(1)
  )
  expect_error(
    fieldforms_output(list(1))
  )
  
  x <- fieldforms_prepare(dplyr::sample_n(top1quad, 3))
  # Passes with warning
  tmp <- tempdir()
  expect_warning(
    fieldforms_output(x, filename = "badish")
  )

  # Passes -- the message comes from `render()`
  expect_message(
    fieldforms_output(x, "forms.docx")
  )
  
  # xxx add here a test for .pdf
  
  expect_error(
    fieldforms_output(x, "forms.docx", header = 1)
  )
})

test_that("outputs a .docx file", {
  x <- fieldforms_prepare(dplyr::sample_n(top1quad, 3))
  filename <- "forms.docx"
  expect_message(
    fieldforms_output(x, filename = filename, output_format = "word_document")
  )
})

unlink("badish")
unlink("forms.docx")



context("fieldforms_prepare")

test_that("fails with wrong arguments", {
  x <- top1quad
  expect_error(
    fieldforms_prepare(x, "a")
  )
  expect_error(
    fieldforms_prepare(x, , , , , "a")
  )
  expect_error(
    fieldforms_prepare(dplyr::select(x, -QX))
  )
  x$CensusID <- 1:2
  expect_error(
    fieldforms_prepare(x)
  )
})
