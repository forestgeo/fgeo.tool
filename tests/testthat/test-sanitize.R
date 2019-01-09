context("sanitize_vft")

vft <- dplyr::sample_n(fgeo.x::vft_4quad, 2)

# Introdue problems to show how to fix them
vft[] <- lapply(vft, as.character)
vft$PlotName <- "NULL"

# Fix
vft_sane <- sanitize_vft(vft)

test_that("fixes NULL and column types", {
  # Bada data
  expect_is(vft$DBH, "character")
  expect_equal(unique(vft$PlotName), "NULL")

  expect_is(vft_sane$DBH, "numeric")
  expect_equal(unique(vft_sane$PlotName), NA_character_)
})

test_that("produces no warning", {
  expect_warning(sanitize_vft(fgeo.x::vft_4quad), NA)
})



context("sanitize_taxa")

taxa <- dplyr::sample_n(read.csv(fgeo.x::example_path("taxa.csv")), 2)
# Creating bad data
taxa[] <- lapply(taxa, as.character)
taxa$SubspeciesID <- "NULL"

taxa_sane <- sanitize_taxa(taxa)

test_that("fixes NULL and column types", {
  # Bad data
  expect_is(taxa$ViewID, "character")
  expect_equal(unique(taxa$SubspeciesID), "NULL")
  # Sanitized
  expect_is(taxa_sane$ViewID, "integer")
  expect_equal(unique(taxa_sane$SubspeciesID), NA_character_)
})

test_that("clarifies missleading message", {
  expect_warning(sanitize_taxa(taxa), NA)
})
