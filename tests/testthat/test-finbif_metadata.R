test_that("fetching metadata works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(
    finbif_use_cache = 1, finbif_cache_path = cache, finbif_rate_limit = Inf
  )

  vcr::use_cassette("finbif_metadata", {

    sources <- finbif_metadata("source", "ru")

    regulatory_status <- finbif_metadata("regulatory_status")

    taxon_rank <- finbif_metadata("taxon_rank")

    country <- finbif_metadata("country")

    region <- finbif_metadata("region")

    bio_province <- finbif_metadata("bio_province")

    bird_assoc_area <- finbif_metadata("bird_assoc_area")

    finnish_occurrence_status <- finbif_metadata("finnish_occurrence_status")

    restriction_level <- finbif_metadata("restriction_level")

  })

  expect_snapshot(sources)

  expect_snapshot(regulatory_status)

  expect_snapshot(taxon_rank)

  expect_snapshot(country)

  expect_snapshot(region)

  expect_snapshot(bio_province)

  expect_snapshot(bird_assoc_area)

  expect_snapshot(finnish_occurrence_status)

  expect_snapshot(restriction_level)

  expect_error(
    finbif_metadata("not_metadata"),
    "not_metadata not found in FinBIF metadata."
  )

  options(finbif_cache_path = NULL)

  options(op)

})
