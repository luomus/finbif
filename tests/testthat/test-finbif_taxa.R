test_that("searching for taxa works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(
    finbif_cache_path = cache,
    finbif_rate_limit = Inf,
    finbif_email = "noreply@laji.fi"
  )

  finbif_clear_cache()

  if (requireNamespace("vcr", quietly = TRUE)) {

    vcr::use_cassette("finbif_taxa", {

      bubo_bubo <- common_name("Bubo bubo")

      otter <- scientific_name("Otter")

      otter_id <- taxon_id("Otter")

    })

    expect_equal(bubo_bubo, "Eurasian Eagle-owl")

    expect_equal(otter, "Lutra lutra")

    expect_equal(otter_id, "MX.47169")

  }

  options(finbif_cache_path = NULL, finbif_email = NULL)

  options(op)

})
