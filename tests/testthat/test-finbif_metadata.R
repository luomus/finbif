test_that("fetching metadata works", {

  skip_on_cran()

  op <- options()

  options(finbif_use_cache = FALSE, finbif_rate_limit = Inf)

  vcr::use_cassette("finbif_metadata", {

    sources <- finbif_metadata("source", "ru")

  })

  expect_snapshot(sources)

  expect_error(
    finbif_metadata("not_metadata"),
    "not_metadata not found in FinBIF metadata."
  )

  options(op)

})
