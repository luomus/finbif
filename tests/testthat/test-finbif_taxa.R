context("Downloading FinBIF taxon info")

vcr::use_cassette(
  "finbif_taxa", {

    test_that(
      "returns valid data", {

        skip_on_cran()

        expect_s3_class(finbif_taxa("Parus major"), "finbif_api")

      }
    )

  },
  preserve_exact_body_bytes = TRUE
)
