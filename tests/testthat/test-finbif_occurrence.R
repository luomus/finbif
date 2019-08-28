context("Querying FinBIF occurrence data")

vcr::use_cassette(
  "finbif_occurrence", {

    test_that(
      "can return valid data", {

        expect_s3_class(
          finbif_occurrence(
            species = "Rangifer tarandus fennicus", check_taxa = FALSE,
            select = "record_id"
          ),
          "finbif_occ"
        )

        expect_s3_class(
          finbif_occurrence(
            "Rangifer tarandus fennicus", "not a valid taxon",
            select = c("record_id", "date_start", "record_fact"),
            on_check_fail = "quiet"
          ),
          "finbif_occ"
        )

        expect_s3_class(
          finbif_occurrence(
            "Rangifer tarandus fennicus",
            select = c("record_id", "date_start", "lat_wgs84", "lon_wgs84")
          ),
          "finbif_occ"
        )

      }

    )

    test_that(
      "can return a count", {

        expect_type(
          finbif_occurrence(
            taxa = "Rangifer tarandus fennicus", count_only = TRUE
          ),
          "integer"
        )

      }

    )

    test_that(
      "returns data that prints/plots valid output", {

        fungi <- finbif_occurrence(
          filter = c(informal_group = "Fungi and lichens"),
          select = c("record_id", "informal_groups", "default_vars"),
          n = 1100L
        )

        expect_output(print(fungi), "Records downloaded:")

        expect_output(
          print(fungi[c("scientific_name", "taxon_rank")]), "A data"
        )

        expect_output(
          print(fungi[1:10, c("scientific_name", "taxon_rank")]), "A data"
        )

        expect_doppelganger("occurrence plot", plot(fungi))

      }
    )

    test_that(
      "warns when taxa invalid", {

        expect_warning(finbif_occurrence("not a valid taxa"))

      }
    )

  },
  preserve_exact_body_bytes = TRUE
)

test_that(
  "returns errors appropriately", {

    expect_error(finbif_occurrence("not a valid taxa", on_check_fail = "error"))

  }
)

