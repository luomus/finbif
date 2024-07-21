test_that("clearing cache works", {

  skip_on_cran()

  op <- options()

  options(finbif_rate_limit = Inf)

  cache <- tempfile()

  dir.create(cache)

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  finbif_clear_cache()

  vcr::use_cassette("finbif_last_mod1", {

    last_mod <- finbif_last_mod()

    finbif_update_cache()

    last_mod <- finbif_last_mod()

  })

  expect_s3_class(last_mod[[1L]], "Date")

  finbif_clear_cache()

  options(finbif_cache_path = cache)

  finbif_clear_cache()

  vcr::use_cassette("finbif_last_mod2", {

    last_mod <- finbif_last_mod()

    finbif_update_cache()

    last_mod <- finbif_last_mod()

  })

  expect_s3_class(last_mod[[1L]], "Date")

  finbif_clear_cache()

  options(finbif_cache_path = db)

  finbif_clear_cache()

  vcr::use_cassette("finbif_last_mod3", {

    last_mod <- finbif_last_mod()

    finbif_update_cache()

    last_mod <- finbif_last_mod()

  })

  expect_s3_class(last_mod[[1L]], "Date")

  finbif_clear_cache()

  options(finbif_cache_path = NULL)

  DBI::dbDisconnect(db)

  options(op)

})
