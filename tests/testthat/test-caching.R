test_that("cache invalidation works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  options(finbif_rate_limit = Inf)

  finbif_clear_cache()

  vcr::use_cassette("finbif_cache_timeout", {

    finbif_occurrence(cache = 1e-09)

    cached <- finbif_occurrence(cache = 1e-09)

  })

  expect_snapshot(cached)

  finbif_clear_cache()

  options(finbif_cache_path = cache)

  vcr::use_cassette("finbif_cache_file_timeout", {

    finbif_occurrence(cache = 1e-09)

    cached <- finbif_occurrence(cache = 1e-09)

  })

  expect_snapshot(cached)

  finbif_clear_cache()

  options(finbif_cache_path = db)

  vcr::use_cassette("finbif_cache_db_timeout", {

    finbif_occurrence(cache = 1e-09)

    cached <- finbif_occurrence(cache = 1e-09)

  })

  expect_snapshot(cached)

  finbif_clear_cache()

  options(finbif_cache_path = NULL)

  DBI::dbDisconnect(db)

  options(op)

})
