test_that("fetching metadata works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(finbif_cache_path = cache, finbif_rate_limit = Inf)

  vcr::use_cassette("finbif_metadata", {

    sources <- finbif_metadata("source", "ru")

  })

  expect_snapshot(sources)

  expect_error(
    finbif_metadata("not_metadata"),
    "not_metadata not found in FinBIF metadata."
  )

  options(finbif_cache_path = NULL)

  options(op)

})
