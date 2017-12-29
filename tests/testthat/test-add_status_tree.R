context("add_status_tree")

fgeo <- tibble::tribble(
  ~CensusID, ~Tag, ~Status,
          1,    1, "alive",
          1,    1,  "dead",
          1,    2,  "dead",
          1,    2,  "dead",
          2,    1, "alive",
          2,    1, "alive",
          2,    2, "alive",
          2,    2,  "dead"
)

test_that("the tree status is dead only if one stem is dead", {
  one_dead <- tibble::tibble(
    tag = c(
      1, 1,
      2, 2,
      3, 3
    ),
    status = c(
      "alive", "dead",
      "dead", "dead",
      "broken below", "missing"
    ),
    censusid = 1,
    plotid = 1
  )
  expected <- c(rep("alive", 2), rep("dead", 2), rep("alive", 2))
  out <- add_status_tree(one_dead)
  expect_equal(out$status_tree, expected)
  expect_is(out, "data.frame")
})

test_that("works even if data already contains the variable `status_tree`", {
  .df <- tibble::tribble(
    ~CensusID, ~Tag,  ~Status,
            1,    1,   "alive",
            1,    1,    "dead",
            1,    2,    "dead",
            1,    2,    "dead",

            2,    1,   "alive",
            2,    1,   "alive",
            2,    2,   "alive",
            2,    2,    "dead"
  )
  expect_silent(add_status_tree(add_status_tree(.df)))
})

test_that("outputs the correct variable `status_tree`", {
  .df <- tibble::tribble(
    ~CensusID, ~Tag,  ~Status,
            1,    1,   "alive",
            1,    1,    "dead",
            1,    2,    "dead",
            1,    2,    "dead",

            2,    1,   "alive",
            2,    1,   "alive",
            2,    2,   "alive",
            2,    2,    "dead"
  )
  exp <- c("alive", "alive", "dead", "dead", "alive", "alive", "alive", "alive")
  expect_identical(add_status_tree(.df)$status_tree, exp)
})
