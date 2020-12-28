suppressMessages(insert_cassette("finbif_collections"))

test_that(
  "returns valid data", {

    skip_on_cran()

    expect_s3_class(
      finbif_collections(taxonomic_coverage == "Coleoptera"), "data.frame"
    )
    expect_s3_class(
      finbif_collections(subcollections = FALSE, supercollections = TRUE),
      "data.frame"
    )
    expect_s3_class(
      finbif_collections(select = NA, nmin = NA), "data.frame"
    )

  }
)

test_that(
  "returns errors appropriately", {

    skip_on_cran()

    expect_condition(finbif_collections(1:50))

  }
)

suppressMessages(eject_cassette("finbif_collections"))
