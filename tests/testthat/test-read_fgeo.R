context("read_vft")

test_that("read_vft fails gracefully if data has missing columns", {
  comma <- tempfile()
  write.csv(fgeo.x::vft_4quad[-1], comma)

  expect_error(read_vft(comma), "DBHID")
})

test_that("read_vft guesses tab or comma separated file", {
  comma <- tempfile()
  write.csv(fgeo.x::vft_4quad, comma)

  expect_silent(
    vft <- read_vft(comma)
  )
  expect_is(vft, "tbl")
})

test_that("read_vft guesses tab or comma separated file", {
  comma <- tempfile()
  readr::write_csv(fgeo.x::vft_4quad, comma)
  expect_silent(
    vft <- read_vft(comma)
  )
  expect_named(vft, names(type_vft()))

  tab <- tempfile()
  readr::write_tsv(fgeo.x::vft_4quad, tab)
  expect_silent(read_vft(tab))
})



context("read_taxa")

test_that("read_taxa can read an online file", {
  skip_on_cran()
  skip_if_not_installed("pingr")
  skip_if(!pingr::is_online(), "Not online.")

  expect_silent(
    taxa <- read_taxa("http://bit.ly/fgeo-data-luquillo-taxa")
  )
  expect_named(taxa, names(type_taxa()))
})

test_that("read_taxa() guesses tab or comma separated file", {
  taxa_tab <- tempfile()

  taxa <- read.csv(fgeo.x::example_path("taxa.csv"))
  readr::write_tsv(taxa, taxa_tab)
  expect_silent(read_taxa(taxa_tab))

  taxa_comma <- tempfile()
  taxa <- read.csv(fgeo.x::example_path("taxa.csv"))
  readr::write_csv(taxa, taxa_comma)
  expect_silent(read_taxa(taxa_comma))
})



context("type_fgeo")

test_that("type_fgeo() outputs a list", {
  expect_type(type_vft(), "list")
  expect_length(type_vft(), 32)
})



context("type_fgeo")

test_that("type_fgeo() outputs a list", {
  expect_type(type_taxa(), "list")
  expect_length(type_taxa(), 21)
})
