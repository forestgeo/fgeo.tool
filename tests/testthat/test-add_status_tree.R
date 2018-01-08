context("add_status_tree")

fgeo <- tribble(
  ~CensusID, ~Tag, ~Status,
          1,    1, "A",
          1,    1,  "D",
          1,    2,  "D",
          1,    2,  "D",
          2,    1, "A",
          2,    1, "A",
          2,    2, "A",
          2,    2,  "D"
)

test_that("the tree status is dead only if one stem is dead", {
  one_dead <- tibble(
    tag = c(
      1, 1,
      2, 2,
      3, 3
    ),
    status = c(
      "A", "D",
      "D", "D",
      "broken below", "missing"
    ),
    censusid = 1,
    plotid = 1
  )
  expected <- c(rep("A", 2), rep("D", 2), rep("A", 2))
  out <- add_status_tree(one_dead, "A", "D")
  expect_equal(out$status_tree, expected)
  expect_is(out, "data.frame")
})

test_that("works even if data already contains the variable `status_tree`", {
  .df <- tribble(
    ~CensusID, ~Tag,  ~Status,
            1,    1,   "A",
            1,    1,    "D",
            1,    2,    "D",
            1,    2,    "D",

            2,    1,   "A",
            2,    1,   "A",
            2,    2,   "A",
            2,    2,    "D"
  )
  expect_silent(add_status_tree(add_status_tree(.df)))
})

test_that("outputs the correct variable `status_tree`", {
  .df <- tribble(
    ~CensusID, ~Tag,  ~Status,
            1,    1,   "A",
            1,    1,    "D",
            1,    2,    "D",
            1,    2,    "D",

            2,    1,   "A",
            2,    1,   "A",
            2,    2,   "A",
            2,    2,    "D"
  )
  exp <- c("A", "A", "D", "D", "A", "A", "A", "A")
  expect_identical(add_status_tree(.df)$status_tree, exp)
})

test_that("warns if the status is invalid", {
  .df <- tribble(
    ~CensusID, ~Tag,  ~Status,
    1,    1,     "alive",
    1,    1,     "dead",
    1,    2,     "dead",
    1,    2,     "dead",

    2,    1,     "alive",
    2,    1,     "alive",
    2,    2,     "alive",
    2,    2,     "dead"
  )
  expect_warning(add_status_tree(.df))
  expect_silent(add_status_tree(.df, "dead", "alive"))
})
