context("Downloading FinBIF records")

test_that(
  "returns valid data", {
    vcr::use_cassette(
      "finbif_records", {
        resp_list_n1 <- finbif_records(n = 1)
        resp_list_n101 <- finbif_records(n = 101)
      },
      preserve_exact_body_bytes = TRUE
    )

    expect_s3_class(resp_list_n1[[1]], "finbif_api")
    expect_s3_class(resp_list_n101[[1]], "finbif_api")
    expect_s3_class(resp_list_n101[[2]], "finbif_api")
    expect_error(finbif_records(n = 1e99), "Cannot download more than")
  }
)
