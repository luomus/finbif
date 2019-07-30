context("FinBIF api methods")

test_that(
  "are working", {
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
        resp_list_admin_status <- finbif_records(
          filters = c(red_list_status = "LC")
        )
        resp_list_fields <- finbif_records(fields = "record_id")
        resp_count <- finbif_records(count_only = TRUE)
      },
      preserve_exact_body_bytes = TRUE
    )

    vcr::use_cassette(
      "finbif_check_taxa", {
        resp_list_sp_true <- finbif_check_taxa(list(species = "Parus major"))
        resp_list_sp_false <- finbif_check_taxa(
          list(species = "Parus notmajor")
        )
        resp_list_rank_false <- finbif_check_taxa(list(genus = "Parus major"))
      },
      preserve_exact_body_bytes = TRUE
    )

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
    expect_output(print(resp_list_n301[[1]]), "FinBIF")
    expect_output(resp_list_n1_2 <- print(resp_list_n1), "FinBIF")
    expect_identical(resp_list_n1_2, resp_list_n1)
    expect_s3_class(as.data.frame(resp_list_n301), "data.frame")
    expect_output(print(resp_list_sp_true), "species: Parus major")
    expect_output(print(resp1), "Records downloaded:")
    expect_output(
      print(resp1[c(1:10, 1), c("scientific_name", "taxon_rank")]), "A data"
    )
    expect_output(print(resp1[c("scientific_name", "taxon_rank")]), "A data")
    expect_type(resp5[["observers_name"]], "list")
  }
)
