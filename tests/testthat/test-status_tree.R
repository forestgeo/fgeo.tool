# library(dplyr)
#
# context("status_tree")
#
# test_that("checks inputs as expected", {
#   expect_error(
#     status_tree(x = "wrong-type")
#   )
#
#   wrong_type <- 1
#   expect_error(
#     status_tree(x = tibble(a = 1), .status = wrong_type)
#   )
#   expect_error(
#     status_tree(x = tibble(missing_name = 1), .status = "correct type")
#   )
#   expect_error(
#     status_tree(x = tibble(status = "A"), .status = "wrong status")
#   )
# })
#
#
# x <- tibble(status_tree = letters[1:3])
#
# test_that("outpus as expected", {
#   expect_equal(status_tree(x, "a")$status_tree, "a")
#   expect_equal(status_tree(x, c("a", "b"))$status_tree, c("a", "b"))
#   expect_equal(status_tree(x, c("a", "b"), exclude = TRUE)$status_tree, "c")
# })
