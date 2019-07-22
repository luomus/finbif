context("Downloading FinBIF records")

test_that(
  "returns valid data", {
    vcr::use_cassette(
      "finbif_records", {
        resp_list_n1 <- finbif_records(n = 1)
        resp_list_n101 <- finbif_records(n = 101)
        resp_list_filters <- finbif_records(filters = c(finnish = TRUE))
        resp_list_informal_group <- finbif_records(
          filters = c(informal_group = "Birds")
        )
        resp_list_admin_status <- finbif_records(
          filters = c(administrative_status = "GMEB")
        )
        resp_list_fields <- finbif_records(fields = "record_id")
        resp_count <- finbif_records(count_only = TRUE)
      },
      preserve_exact_body_bytes = TRUE
    )

    expect_s3_class(resp_list_n1[[1]], "finbif_api")
    expect_s3_class(resp_list_n101[[1]], "finbif_api")
    expect_s3_class(resp_list_n101[[2]], "finbif_api")
    expect_s3_class(resp_list_fields[[1]], "finbif_api")
    expect_s3_class(resp_list_filters[[1]], "finbif_api")
    expect_s3_class(resp_list_informal_group[[1]], "finbif_api")
    expect_s3_class(resp_list_admin_status[[1]], "finbif_api")
    expect_s3_class(resp_count, "finbif_api")
    expect_error(finbif_records(n = 1e99), "Cannot download more than")
    expect_error(
      finbif_records(filters = c(not_a_filter = TRUE)), "Invalid filter name"
    )
    expect_error(
      finbif_records(filters = c(informal_group = "Birbs")),
      "Invalid informal group"
    )
    expect_error(
      finbif_records(filters = c(administrative_status = "not a status")),
      "Invalid administrative status"
    )
    expect_error(finbif_records(fields = "not_a_field"), "Invalid field name")

  }
)
