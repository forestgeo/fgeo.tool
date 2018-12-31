context("input_list")

test_that("outputs expected object", {
  dir <- fgeo.x::example_path("csv")
  expect_is(csv_list(dir), "list")
  
  dir <- fgeo.x::example_path("rds")
  expect_is(rds_list(dir), "list")
  
  dir <- fgeo.x::example_path("rdata")
  expect_is(rdata_list(dir), "list")
})

test_that("can read specific files in a mixed directory", {
  dir <- fgeo.x::example_path("mixed_files")
  expect_is(csv_list(dir), "list")
})



context("read_with")

test_that("read_with() can handle cero .rdata files", {
  zero <- fgeo.x::example_path("csv")
  expect_error(rdata_list(zero), "Can't find.*rdata")
  
  zero <- fgeo.x::example_path("rdata")
  expect_error(read_with(readr::read_csv, "[.]csv")(zero), "Can't find.*csv")
})

