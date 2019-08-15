context("Downloading FinBIF taxon info")

vcr::use_cassette(
  "finbif_taxa", {

    test_that(
      "returns valid data", {

        expect_s3_class(finbif_taxa("Parus major"), "finbif_api")

      }
    )

  },
  preserve_exact_body_bytes = TRUE
)
