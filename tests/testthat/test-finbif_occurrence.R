context("Downloading FinBIF occurrence data")

test_that(
  "returns valid data", {
    vcr::use_cassette(
      "finbif_occurrence", {
        resp1 <- finbif_occurrence(taxa = "Parus major")
        resp2 <- finbif_occurrence(species = "Parus major")
      },
      preserve_exact_body_bytes = TRUE
    )

    expect_s3_class(resp1, "data.frame")
    expect_s3_class(resp2, "data.frame")
  }
)
