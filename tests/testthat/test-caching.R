test_that("cache invalidation works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(finbif_rate_limit = Inf)

  finbif_clear_cache()

  if (requireNamespace("vcr", quietly = TRUE)) {

    vcr::use_cassette("finbif_cache_timeout", {

      finbif_occurrence(cache = 1e-09)

      cached <- finbif_occurrence(cache = TRUE)

    })

    expect_snapshot(cached)

  }

  finbif_clear_cache()

  options(finbif_cache_path = cache)

  if (requireNamespace("vcr", quietly = TRUE)) {

    vcr::use_cassette("finbif_cache_file_timeout", {

      finbif_occurrence(cache = 1e-09)

      cached <- finbif_occurrence(cache = TRUE)

    })

    expect_snapshot(cached)

  }

  finbif_clear_cache()

  if (
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)
  ) {

    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    options(finbif_cache_path = db)

  }

  if (requireNamespace("vcr", quietly = TRUE)) {

    vcr::use_cassette("finbif_cache_db_timeout", {

      finbif_occurrence(cache = 1e-09)

      cached <- finbif_occurrence(cache = TRUE)

    })

    expect_snapshot(cached)

  }

  finbif_clear_cache()

  options(finbif_cache_path = NULL)

  if (requireNamespace("DBI", quietly = TRUE)) {

    DBI::dbDisconnect(db)

  }

  options(op)

})
