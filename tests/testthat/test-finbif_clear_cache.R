test_that("clearing cache works", {

  expect_null(finbif_clear_cache())

  op <- options()

  options(finbif_cache_path = tempdir())

  expect_null(finbif_clear_cache())

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbCreateTable(db, "finbif_cache", c(hash = "TEXT"))

  options(finbif_cache_path = db)

  expect_null(finbif_clear_cache())

  options(op)

})
