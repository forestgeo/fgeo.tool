context("input_list")

test_that("outputs expected object", {
  dir <- fgeo.x::path_example("csv")
  expect_is(csv_list(dir), "list")
  
  dir <- fgeo.x::path_example("rds")
  expect_is(rds_list(dir), "list")
  
  dir <- fgeo.x::path_example("rdata")
  expect_is(rdata_list(dir), "list")
})

test_that("can read specific files in a mixed directory", {
  dir <- fgeo.x::path_example("mixed_files")
  expect_is(csv_list(dir), "list")
})



context("read_with")

test_that("read_with() can handle cero .rdata files", {
  zero <- fgeo.x::path_example("csv")
  expect_error(rdata_list(zero), "Can't find.*rdata")
  
  zero <- fgeo.x::path_example("rdata")
  expect_error(read_with(readr::read_csv, "[.]csv")(zero), "Can't find.*csv")
})

