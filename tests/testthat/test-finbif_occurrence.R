context("Downloading FinBIF occurrence data")

test_that(
  "returns valid data", {
    vcr::use_cassette(
      "finbif_occurrence", {
        resp1 <- finbif_occurrence(taxa = "Parus major")
        resp2 <- finbif_occurrence(species = "Parus major")
        resp3 <- finbif_occurrence(species = "Parus major", check_taxa = FALSE)
        resp4 <- finbif_occurrence(taxa = "Parus major", count_only = TRUE)
        resp5 <- finbif_occurrence(
          "Parus major", fields = c("record_id", "observers_name")
        )
        resp6 <- finbif_occurrence(
          "Parus major", fields = c("record_id", "date_start")
        )
        resp7 <- finbif_occurrence(
          "Parus major",
          fields = c("record_id", "date_start", "lat_wgs84", "lon_wgs84")
        )
      },
      preserve_exact_body_bytes = TRUE
    )

    expect_s3_class(resp1, "data.frame")
    expect_s3_class(resp2, "data.frame")
    expect_s3_class(resp3, "data.frame")
    expect_type(resp4, "integer")
    expect_s3_class(resp5, "finbif_occ")
    expect_s3_class(resp6, "finbif_occ")
    expect_s3_class(resp7, "finbif_occ")
    expect_s3_class(resp7[["date_time"]], "POSIXct")
    expect_type(resp4, "integer")
  }
)

test_that(
  "returns the expected number of records", {
    vcr::use_cassette(
      "finbif_occurrence500", {
        resp500 <- finbif_occurrence(n = 500)
      },
      preserve_exact_body_bytes = TRUE
    )
    expect_identical(500L, nrow(resp500))

  }
)
