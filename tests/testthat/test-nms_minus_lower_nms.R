context("nms_minus_lower_nms.R")

test_that("returns expected value", {
  skip_on_travis()
  pkg_yosemite_is_not_available <- !require(yosemite)
  skip_if(pkg_yosemite_is_not_available, "For privacy, yosemite is only local")

  vft <- yosemite::ViewFullTable_yosemite
  stem <- yosemite::yosemite_s1_lao
  tree <- yosemite::yosemite_f1_lao

  expect_equal(nms_minus_lower_nms(vft, stem), 7)
  expect_identical(
    nms_minus_lower_nms(stem, vft),
    nms_minus_lower_nms(vft, stem)
  )
})
