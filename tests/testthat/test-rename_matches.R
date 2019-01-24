context("rename_matches")

describe("detect_insensitive()", {
  it("returns true in position of x where x matches y", {
    expect_equal(detect_insensitive(letters[1:3], LETTERS[3:5]), c(F, F, T))
    expect_equal(detect_insensitive(letters[1:3], letters[3:5]), c(F, F, T))
    expect_equal(detect_insensitive(LETTERS[1:3], LETTERS[3:5]), c(F, F, T))

    expect_equal(detect_insensitive(letters[1], LETTERS[3:5]), c(F))
    expect_equal(detect_insensitive(letters[1:3], LETTERS[3]), c(F, F, T))
    expect_equal(detect_insensitive(letters[1], LETTERS[4:5]), c(F))
  })

  it("outputs as expected with fgeo data", {
    x <- c("stemid", "n")
    y <- c("StemID", "treeID")
    expect_equal(detect_insensitive(x, y), c(TRUE, FALSE))
  })

  it("fails gracefully", {
    expect_error(detect_insensitive(1, "a"), "is not TRUE")
    expect_error(extract_insensitive(1), "argument.*is missing")
    expect_error(detect_insensitive(data.frame(1), "a"), "is not TRUE")
    expect_error(extract_insensitive(data.frame(1)), "argument.*is missing")
  })
})

describe("extract_insensitive()", {
  it("extracts the element of x that matches insensitive y", {
    aa <- c("a", "a")
    AA <- c("A", "A")
    bb <- c("b", "b")
    BB <- c("B", "B")
    ab <- c("a", "b")
    aB <- c("a", "B")
    Ab <- c("A", "b")
    AB <- c("A", "B")
    ba <- c("b", "a")
    BA <- c("B", "A")

    expect_equal(extract_insensitive(aa, ab), aa)
    expect_equal(extract_insensitive(aa, AB), AA)
    expect_equal(extract_insensitive(ab, ba), c("a", "b"))

    # Position
    expect_equal(extract_insensitive(ab, Ab), Ab)
    expect_equal(extract_insensitive(ab, aB), aB)
    # Length
    expect_equal(extract_insensitive("a", AB), "A")

    expect_equal(extract_insensitive(ab, "A"), Ab)
    # Duplicated
    expect_equal(extract_insensitive(aa, AB), AA)
    expect_equal(extract_insensitive("a", AA), "A")
    expect_equal(extract_insensitive("a", "b"), "a")
    expect_equal(extract_insensitive(aa, bb), aa)
    expect_equal(extract_insensitive("a", bb), "a")

    # Missing values
    expect_equal(extract_insensitive("a", NA_character_), "a")
    expect_equal(extract_insensitive(NA, "a"), NA_character_)
    expect_equal(extract_insensitive(NA, NA_character_), NA_character_)
    aNA <- c("a", NA)
    ANA <- c("A", NA)
    expect_equal(extract_insensitive(aNA, "A"), ANA)
    expect_equal(extract_insensitive("a", ANA), "A")
  })

  it("outputs as expected with fgeo data", {
    x <- c("stemid", "n")
    y <- c("StemID", "treeID")
    expect_equal(extract_insensitive(x, y), c("StemID", "n"))
  })
})

describe("rename_matches()", {
  it("outputs correct names", {

    # Problematic datasets
    out <- data.frame(
      stringsAsFactors = FALSE,
      plotname = c("luq", "luq"),
      year = c(2001, 2002),
      family = c("f", "f"),
      species = c("Gn spp", "Gn spp"),
      n = c(2L, 1L)
    )

    ref <- data.frame(
      stringsAsFactors = FALSE,
      plotname = c("luq", "luq"),
      censusid = c(1L, 1L),
      treeid = c(2L, 1L),
      stemid = c(2.2, 1.1),
      status = c("alive", "alive"),
      dbh = c(3L, 1L),
      genus = c("Gn", "Gn"),
      speciesname = c("spp", "spp"),
      exactdate = c("2001-01-01", "2001-01-01"),
      plotcensusnumber = c(1L, 1L),
      family = c("f", "f"),
      tag = c(2L, 1L),
      status_tree = c("alive", "alive"),
      year = c(2001, 2001),
      species = c("Gn spp", "Gn spp")
    )

    expect_equal(rename_matches(out, ref)$year, c(2001, 2002))
    expect_equal(rename_matches(out, ref)$family, c("f", "f"))
  })
})

test_that("works as expected", {
  x <- data.frame(col1 = 5, col2 = 1, n = 5)

  y <- data.frame(COL1 = 1, COL2 = 1, COL3 = 1)
  expect_named(rename_matches(x, y), c("COL1", "COL2", "n"))

  y <- data.frame(COL1 = 1)
  expect_named(rename_matches(x, y), c("COL1", "col2", "n"))

  y <- data.frame(NOMATCH = 1)
  expect_named(rename_matches(x, y), c("col1", "col2", "n"))

  y <- data.frame(1)
  expect_named(rename_matches(x, y), c("col1", "col2", "n"))
})
