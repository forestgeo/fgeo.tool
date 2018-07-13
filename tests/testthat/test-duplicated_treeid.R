context("duplicated_treeid")

test_that("warns when appropriate", {
  expect_warning(
    warn_duplicated_treeid(fgeo.data::luquillo_stem6_1ha),
    "Detected duplicated `treeID`"
  )
  expect_silent(warn_duplicated_treeid(fgeo.data::luquillo_tree6_1ha))
  
  combo56 <- purrr::reduce(
    list(fgeo.data::luquillo_tree5_random, fgeo.data::luquillo_tree5_random),
    rbind
  )
  expect_silent(warn_duplicated_treeid(combo56))
})
