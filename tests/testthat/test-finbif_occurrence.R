context("Downloading FinBIF occurrence data")

test_that(
  "returns valid data", {
    vcr::use_cassette(
      "finbif_occurrence", {
        resp1 <- finbif_occurrence(taxa = "Parus major")
        resp2 <- finbif_occurrence(species = "Parus major")
        resp3 <- finbif_occurrence(species = "Parus major", check_taxa = FALSE)
        resp4 <- finbif_occurrence(taxa = "Parus major", count_only = TRUE)
      },
      preserve_exact_body_bytes = TRUE
    )

    expect_s3_class(resp1, "data.frame")
    expect_s3_class(resp2, "data.frame")
    expect_s3_class(resp3, "data.frame")
    expect_type(resp4, "integer")
  }
)
