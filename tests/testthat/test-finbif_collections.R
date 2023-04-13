suppressMessages(insert_cassette("finbif_collections"))

test_that(
  "returns valid data", {

    skip_on_cran()

    expect_s3_class(
      finbif_collections(taxonomic_coverage == "Coleoptera", cache = 1),
      "data.frame"
    )

    expect_s3_class(
      finbif_collections(subcollections = FALSE, supercollections = TRUE),
      "data.frame"
    )

    op <- getOption("finbif_cache_path")

    options(finbif_cache_path = tempdir())

    expect_s3_class(
      finbif_collections(select = NA, nmin = NA, cache = 1e-9), "data.frame"
    )

    expect_s3_class(
      finbif_collections(select = NA, nmin = NA, cache = 1e-9), "data.frame"
    )

    options(finbif_cache_path = op)

  }
)

test_that(
  "returns errors appropriately", {

    skip_on_cran()

    expect_condition(finbif_collections(1:50))

  }
)

suppressMessages(eject_cassette("finbif_collections"))
