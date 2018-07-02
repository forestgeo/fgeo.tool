context("add_subquad")

test_that("passes with non-numeric input", {
  vft <- tibble::tribble(
     ~QX,  ~QY,
    NULL,    0,
    17.9,    0
  ) %>%
    purrr::map_df(as.character)

  expect_warning(
    add_subquad(vft, x_q = 20, x_sq = 5)
  )

  vft <- tibble::tribble(
     ~QX,  ~QY,
    17.9,    0,
     4.1,   15,
     6.1, 17.3
  ) %>%
    purrr::map_df(as.character)

  expect_warning(add_subquad(vft, x_q = 20, x_sq = 5))
})

vft <- tibble::tribble(
  ~QX,  ~QY,
  17.9,    0,
  4.1,   15,
  6.1, 17.3,
  3.8,  5.9,
  4.5, 12.4,
  4.9,  9.3,
  9.8,  3.2,
  18.6,  1.1,
  17.3,  4.1,
  1.5, 16.3
)

test_that("outputs a dataframe with new expected variable", {
  with_sq <- vft %>% add_subquad(x_q = 20, x_sq = 5)
  expect_named(with_sq, c(names(vft), "subquadrat"))
  expect_is(with_sq, "data.frame")
})

test_that("throws error with wrong inputs to add_subquad", {

  # check that works
  expect_silent(add_subquad(vft, 20, 20, 5, 5))
  expect_silent(add_subquad(vft, 40, 50, 5, 5))

  # Fails
  expect_error(add_subquad(1, 20, 20, 5, 5))
  expect_error(add_subquad(vft, "a", 20, 5, 5))
  expect_error(add_subquad(vft, 20, c(20, 20), 5, 5))
  expect_error(add_subquad(vft, 20, 20, "a", 5))
  expect_error(add_subquad(vft, 20, 20, 5, c(5, 5)))
  expect_error(add_subquad(vft, -1, 20, 5, 5))
  expect_error(add_subquad(vft, 20, Inf, 5, 5))
  expect_error(add_subquad(vft, 20, 20, 5, 5, subquad_offset = "wrong input"))
})

context("recode_subquad")

with_subquad <- tibble(subquadrat = c("01", "02", "12"))

test_that("recodes as expected", {
  at_origin_1 <- recode_subquad(with_subquad, 1)
  expect_equal(at_origin_1$subquadrat, c("11", "12", "22"))
  at_origin_0 <- recode_subquad(at_origin_1, offset = -1)
})

test_that("errs with wrong input", {
  expect_error(
    recode_subquad("not a dataframe")
  )
  expect_error(
    recode_subquad(data.frame(x = "missing var subquadrat"))
  )
  expect_error(
    recode_subquad(with_subquad, 9999)
  )
  expect_error(
    recode_subquad(tibble(subquadrat = c("11", "wrong subquad")))
  )
})
