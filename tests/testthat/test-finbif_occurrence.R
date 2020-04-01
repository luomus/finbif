context("Querying FinBIF occurrence data")

vcr::use_cassette(
  "finbif_occurrence", {

    test_that(
      "can return valid data", {

        skip_on_cran()

        expect_s3_class(
          finbif_occurrence(
            species = "Rangifer tarandus fennicus", check_taxa = FALSE,
            select = "record_id", sample = TRUE
          ),
          "finbif_occ"
        )

        expect_s3_class(
          finbif_occurrence(
            "Rangifer tarandus fennicus", "not a valid taxon",
            select = c("record_id", "date_start", "record_fact_content"),
            check_taxa = FALSE
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

        expect_s3_class(
          finbif_occurrence(
            "Pteromys volans", select = c("default_vars", "duration"),
            sample = TRUE, n = 1001, cache = FALSE
          ),
          "finbif_occ"
        )

        expect_s3_class(
          finbif_occurrence("Vulpes vulpes", sample = TRUE, n = 1600),
          "finbif_occ"
        )

        expect_s3_class(
          finbif_occurrence(select = "taxon_id", sample = TRUE, n = 3001),
          "finbif_occ"
        )

      }

    )

    test_that(
      "can return a count", {

        skip_on_cran()

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

        skip_on_cran()

        fungi <- finbif_occurrence(
          filter = c(informal_group = "Fungi and lichens"),
          select = c(
            "record_id", "informal_groups", "taxon_id", "default_vars"
          ),
          n = 1100L
        )

        expect_output(print(fungi), "Records downloaded:")

        expect_output(
          print(fungi[c("scientific_name", "taxon_id")]), "A data"
        )

        expect_output(
          print(fungi[1:10, c("scientific_name", "taxon_id")]), "A data"
        )

        expect_doppelganger(
          paste0("occurrence plot ", if (is_dev_api()) "dev"),
          plot(fungi)
        )

        options(finbif_cache_path = tempdir())

        expect_output(
          print(
            finbif_occurrence(
              select = c("default_vars", "duration"), dwc = TRUE
            )
          ),
          "Records downloaded:"
        )

        expect_output(print(finbif_occurrence()), "Records downloaded:")

      }
    )

    test_that(
      "warns when taxa invalid", {

        skip_on_cran()

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
