context("Downloading FinBIF taxon info")

test_that(
  "returns valid data", {
    vcr::use_cassette(
      "finbif_taxa", {
        resp_list <- finbif_taxa("Parus major")
      },
      preserve_exact_body_bytes = TRUE
    )

    expect_s3_class(resp_list, "finbif_api")

  }
)
