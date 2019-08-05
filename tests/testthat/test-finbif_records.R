context("Downloading FinBIF records")

test_that(
  "returns valid data", {
    vcr::use_cassette(
      "finbif_records", {
        resp_list_n1 <- finbif_records(n = 1)
        resp_list_n301 <- finbif_records(n = 301)
        resp_list_filters <- finbif_records(filters = c(finnish = TRUE))
        resp_list_informal_group <- finbif_records(
          filters = c(informal_group = "Birds")
        )
        resp_list_admin_status <- finbif_records(
          filters = c(administrative_status = "GMEB")
        )
        resp_list_red_list_status <- finbif_records(
          filters = c(red_list_status = "LC")
        )
        resp_list_habitat <- finbif_records(
          filters = list(primary_habitat = list(M = c("V", "H")))
        )
        resp_list_taxon_rank <- finbif_records(
          filters = c(taxon_rank = "species")
        )
        resp_list_country <- finbif_records(filters = c(country = "Finland"))
        resp_list_fields <- finbif_records(fields = "record_id")
        resp_count <- finbif_records(count_only = TRUE)
      },
      preserve_exact_body_bytes = TRUE
    )

    expect_s3_class(resp_list_n1[[1]], "finbif_api")
    expect_s3_class(resp_list_n301[[1]], "finbif_api")
    expect_s3_class(resp_list_n301[[2]], "finbif_api")
    expect_s3_class(resp_list_fields[[1]], "finbif_api")
    expect_s3_class(resp_list_filters[[1]], "finbif_api")
    expect_s3_class(resp_list_informal_group[[1]], "finbif_api")
    expect_s3_class(resp_list_admin_status[[1]], "finbif_api")
    expect_s3_class(resp_list_red_list_status[[1]], "finbif_api")
    expect_s3_class(resp_list_habitat[[1]], "finbif_api")
    expect_s3_class(resp_list_taxon_rank[[1]], "finbif_api")
    expect_s3_class(resp_list_country[[1]], "finbif_api")
    expect_s3_class(resp_count, "finbif_api")
    expect_error(finbif_records(n = 1e99), "Cannot download more than")
    expect_error(finbif_records(filters = c(not_a_filter = TRUE)), "Invalid")
    expect_error(
      finbif_records(filters = c(informal_group = "Birbs")), "Invalid"
    )
    expect_error(
      finbif_records(filters = c(administrative_status = "not a status")),
      "Invalid"
    )
    expect_error(
      finbif_records(filters = c(red_list_status = "not a status")), "Invalid"
    )
    expect_error(finbif_records(fields = "not_a_field"), "Invalid")

  }
)
