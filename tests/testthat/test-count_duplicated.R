context("count_duplicated.R")

test_that("works if grouping by variable called n", {
  df <- data.frame(n = c(1, 1, 1, 2, 2, 3))
  
  # Count by n then filter by nn
  w_dplyr <- dplyr::count(df, n) %>% dplyr::filter(nn > 1)
  out <- count_duplicated(df, n)
  expect_equal(out, w_dplyr)
  
  # Count by n then filter by n
  w_dplyr <- dplyr::count(df, n) %>% dplyr::filter(n > 1)
  # This should filter by nn, so different.
  out <- count_duplicated(df, n)
  expect_false(identical(out, w_dplyr))
  
  expect_equal(names(out), c("n", "nn"))
  expect_equal(out$nn, c(3, 2))
  
  # Sorting works
  df <- data.frame(n = c(3, 3, 3, 2, 2, 1))
  out <- df %>% count_duplicated(n, sort = TRUE)
  expect_equal(out$nn, c(3, 2))
  
  out <- df %>% count_duplicated(n, sort = FALSE)
  expect_equal(out$nn, c(2, 3))
})

test_that("Outputs the same as without dplyr", {
  # Group by one var
  df <- data.frame(a = c(1, 2, 2, 3, 3, 3))
  w_dplyr <- dplyr::count(df, a) %>% dplyr::filter(n > 1)
  out <- count_duplicated(df, a)
  expect_equal(out, w_dplyr)
  expect_equal(out$n, c(2, 3))
  
  # Group by more than one var
  df <- data.frame(a = c(1, 2, 2, 3, 3, 3), b = c(1, 1, 1, 2, 2, 2))
  w_dplyr <- dplyr::count(df, a, b) %>% dplyr::filter(n > 1)
  out <- count_duplicated(df, a, b)
  expect_equal(out, w_dplyr)
  expect_equal(out$n, c(2, 3))
})

# From dplyr::count
test_that("warns and ungroups if detects grouped input", {
  df <- data.frame(g = c(1, 2, 2, 2))
  
  out1 <- count_duplicated(df, g)
  expect_equal(dplyr::group_vars(out1), character())
  
  df <- dplyr::group_by(df, g)
  expect_warning({out2 <- count_duplicated(df, g)})
  expect_equal(dplyr::group_vars(out2), character())
})

