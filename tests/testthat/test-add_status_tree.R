context("add_status_tree")

stem <- tribble(
  ~CensusID, ~treeID, ~stemID, ~status,
          1,       1,       1,     "A",
          1,       1,       2,     "D",
          
          1,       2,       3,     "D",
          1,       2,       4,     "D",
          # ++++++++++++++++++++++++++
          2,       1,       1,     "A",
          2,       1,       2,     "G",
          
          2,       2,       3,     "D",
          2,       2,       4,     "G"
)

test_that("outputs with expected names", {
  out <- add_status_tree(stem)
  expect_named(out, c("CensusID", "treeID", "stemID", "status", "status_tree"))
})

test_that("determines the correct status of a stem", {
  .stem <- add_status_tree(stem)
  expect_equal(.stem$status_tree, c("A", "A", "D", "D", "A", "A", "A", "A"))
  
  .stem1 <- add_status_tree(filter(stem, CensusID == 1))
  expect_equal(.stem1$status_tree, c("A", "A", "D", "D"))
})



test_that("the tree status is dead only if one stem is dead", {
  one_dead <- tibble(
    TreeID = c(
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
    ~CensusID, ~TreeID,  ~Status,
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
    ~CensusID, ~TreeID,  ~Status,
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
    ~CensusID, ~TreeID,  ~Status,
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

test_that("handles names as in viewfull (vf) and census (cns) tables", {
  vf <- tibble::tribble(
    ~CensusID, ~TreeID, ~Status,
                1,    2,  "A",
                2,    2,  "D"
  )
  expect_silent(add_status_tree(vf))

  cns <- tibble::tribble(
    ~CensusID, ~TreeID, ~status,
                1,    2,  "A",
                2,    2,  "D"
  )
  expect_silent(add_status_tree(cns))
})

test_that("names of data are equal in input and output, except status_tree", {
  cns <- tibble::tribble(
    ~CensusID, ~TreeID, ~status,
                1,    2,  "A",
                2,    2,  "D"
  )
  nms_in <- names(cns)
  nms_out <- names(add_status_tree(cns))
  expect_identical(nms_in, nms_out[1:3])
})
