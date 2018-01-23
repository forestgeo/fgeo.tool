context("discard_dead_twice")

vft <- tibble::tribble(
  ~CensusID, ~Tag,     ~Status,
          0,    1,      "dead",  # this is irrelevant; census too early
          0,    1,      "dead",  
          
          1,    1,      "dead",  # tag 1 should stay; tree is dead only in cns 2
          1,    1,      "alive",
          
          2,    1,      "dead",
          2,    1,      "dead",
          
          
          
          0,    2,      "dead",  # this is irrelevant; census too early
          0,    2,      "dead",
          
          1,    2,      "dead",  # tag 2 should go; tree is dead in cns 1 & 2
          1,    2,      "dead",
          
          2,    2,      "dead",
          2,    2,      "dead"
)

test_that("correctly removes trees found dead in two censuses", {
  actual <- discard_dead_twice(add_status_tree(vft, "alive", "dead"))$status_tree
  expected <- c("alive", "alive",  "dead", "dead") 
  expect_equal(actual, expected)
})

test_that("works with with lowercase names", {
  vft <- add_status_tree(vft, "alive", "dead")
  expect_silent(
    discard_dead_twice(
      set_names(vft,tolower)
      )
  )
})

test_that("requires `status_tree", {
  expect_error(
    discard_dead_twice(vft)
  )
})
