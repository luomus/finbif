test_that("clearing cache works", {

  op <- options()

  expect_null(finbif_clear_cache())

  cache <- tempfile()

  dir.create(cache)

  options(finbif_cache_path = cache)

  expect_null(finbif_clear_cache())

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbCreateTable(db, "finbif_cache", c(hash = "TEXT"))

  options(finbif_cache_path = db)

  expect_null(finbif_clear_cache())

  options(finbif_cache_path = NULL)

  DBI::dbDisconnect(db)

  options(op)

})
