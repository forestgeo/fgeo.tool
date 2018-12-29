context("pick")

dfm_ls <- list(
  c1 = tibble::tibble(dbh = 1:2),
  c2 = tibble::tibble(dbh = 8:9)
)

test_that("errs if dataframes differ in dimensions", {
  expect_error(
    pick(list(
      c1 = tibble(x = 1),
      c2 = tibble(x = 1:2)
    )),
    "All dataframes must have the same number of rows and columns"
  )
  
  expect_error(
    pick(list(
      c1 = tibble(x = 1),
      c2 = tibble(x = 1, y = 1)
    )),
    "All dataframes must have the same number of rows and columns"
  )
})

# test_that("errs if dataframes differ in names", {
#   expect_error(
#     pick(list(
#       c1 = tibble(x = 1),
#       c2 = tibble(y = 1)
#     )),
#     "All dataframes must have the same names"
#   )
# })


test_that("with a list of dataframes returns the expected structure", {
  expect_is(pick(dfm_ls), "list")
  expect_equivalent(pick(dfm_ls), dfm_ls)
  
  expect_false(
    isTRUE(all.equal(
      pick(dfm_ls, dbh == 1)[[1]], 
      dfm_ls[[1]]
    ))
  )
  
  expect_length(pick(dfm_ls), 2)
  expect_named(pick(dfm_ls), c("c1", "c2"))
  expect_named(pick(dfm_ls)[[1]], c("dbh"))
  
})

test_that("errs with informative message", {
  expect_error(pick(1), "Can't deal with data")
  expect_error(pick(tibble::tibble(x = 1)), "Can't deal with data")
  expect_error(pick(list(1)), "is.data.frame.*not TRUE")
  expect_error(pick(dfm_ls, novar > 1), "'novar' not found")
  expect_error(pick(dfm_ls, dbh == 1, key = "bad"), "'bad' not found")
})

test_that("picking a row in key census picks the same row in other censuses", {
  out <- pick(dfm_ls, dbh == 1, key = "c1")
  expect_equal(out[[1]]$dbh, 1)
  expect_equal(out[[2]]$dbh, 8)
  
  out <- pick(dfm_ls, dbh == 2, key = "c1")
  expect_equal(out[[1]]$dbh, 2)
  expect_equal(out[[2]]$dbh, 9)

  out <- pick(dfm_ls, dbh == 9, key = "c2")
  expect_equal(out[[1]]$dbh, 2)
  expect_equal(out[[2]]$dbh, 9)
  
  out <- pick(dfm_ls, dbh == 9 | dbh < 9, key = "c2")
  expect_equal(out[[1]]$dbh, 1:2)
  expect_equal(out[[2]]$dbh, 8:9)
  
  out <- pick(dfm_ls, dbh == 0, key = "c1")
  expect_equal(out[[1]]$dbh, integer(0))
  expect_equal(out[[2]]$dbh, integer(0))
})

