context("read_fgeo")

test_that("read_fgeo() reads data correctly", {
  url_vft <- "http://bit.ly/fgeo-data-luquillo-vft-random"
  tryCatch(
    vft <- read_vft(url_vft),
    error = function(e) {
      skip("Are you connected to internet?")
    }
  )
  
  expect_equal(names(vft), names(type_vft()))

  url_taxa <- "http://bit.ly/fgeo-data-luquillo-taxa"
  expect_silent(taxa <- read_taxa(url_taxa))
  expect_equal(names(taxa), names(type_taxa()))
  
  taxa_readr <- readr::read_delim(url_taxa, "\t")
  expect_equal(names(taxa), names(taxa_readr))
})



test_that("type_fgeo() outputs a list", {
  expect_type(type_vft(), "list")
  expect_length(type_vft(), 32)
  expect_type(type_taxa(), "list")
  expect_length(type_taxa(), 21)
})



test_that("guess_comma_or_tab() guesses tab or comma separated file", {
  expect_equal(guess_comma_or_tab("a,b\n1,1", nms = c("a", "b")), ",")
  expect_equal(guess_comma_or_tab("a\tb\n1\t1", nms = c("a", "b")), "\t")
  
  comma <- tempfile()
  readr::write_csv(fgeo.data::luquillo_vft_4quad, comma)
  expect_equal(guess_comma_or_tab(comma, nms = names(type_vft())), ",")
  expect_silent(guess_comma_or_tab(comma, nms = names(type_vft())))
  
  tab <- tempfile()
  readr::write_tsv(fgeo.data::luquillo_vft_4quad, tab)
  expect_equal(guess_comma_or_tab(tab, nms = names(type_vft())), "\t")
  
  expect_silent(read_vft(tab))
  expect_silent(read_vft(comma))
  
  taxa_tab <- tempfile()
  readr::write_tsv(fgeo.data::luquillo_taxa, taxa_tab)
  expect_silent(read_taxa(taxa_tab))
  
  taxa_comma <- tempfile()
  readr::write_csv(fgeo.data::luquillo_taxa, taxa_comma)
  expect_silent(read_taxa(taxa_comma))
})
