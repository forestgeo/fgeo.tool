context("test-read_fgeo")

test_that("reads data correctly", {
  url_vft <- "http://bit.ly/fgeo-data-luquillo-vft-random"
  tryCatch(
    vft <- read_vft(url_vft),
    error = function(e) {
      skip("Are you connected to internet?")
    }
  )
  
  expect_equal(names(vft), names(type_vft()))

  url_taxa <- "http://bit.ly/fgeo-data-luquillo-taxa"
  expect_warning(taxa <- read_vft(url_taxa))
  expect_equal(names(taxa), names(type_taxa()))
  
  taxa_readr <- readr::read_delim(url_taxa, "\t")
  expect_equal(names(taxa), names(taxa_readr))
})



context("type_fgeo.R")

test_that("outputs a list", {
  expect_type(type_vft(), "list")
  expect_length(type_vft(), 32)
  expect_type(type_taxa(), "list")
  expect_length(type_taxa(), 21)
})
