context("Downloading FinBIF collection info")

vcr::use_cassette(
  "finbif_collections", {

    test_that(
      "returns valid data", {

        expect_s3_class(finbif_collections(), "data.frame")
        expect_s3_class(finbif_collections(TRUE), "data.frame")
        expect_s3_class(finbif_collections(TRUE, FALSE), "data.frame")
        expect_s3_class(finbif_collections(nmin = NA), "data.frame")

      }
    )

  },
  preserve_exact_body_bytes = TRUE
)
