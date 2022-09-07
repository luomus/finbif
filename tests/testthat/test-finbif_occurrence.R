suppressMessages(insert_cassette("finbif_occurrence"))

test_that(
  "can return valid data", {

    skip_on_cran()

    expect_s3_class(
      finbif_occurrence(
        species = "Rangifer tarandus fennicus", check_taxa = FALSE,
        select = "municipality", sample = TRUE
      ),
      "finbif_occ"
    )

    expect_s3_class(
      finbif_occurrence(
        "Rangifer tarandus fennicus", "not a valid taxon",
        select = c("record_id", "date_start", "record_fact_name"),
        check_taxa = FALSE
      ),
      "finbif_occ"
    )

    expect_s3_class(
      finbif_occurrence(
        "Rangifer tarandus fennicus",
        select = c("record_id", "date_start", "lat_wgs84", "lon_wgs84"),
        exclude_na = TRUE
      ),
      "finbif_occ"
    )

    capture.output(
      with_progress <- suppressMessages(
        finbif_occurrence(
          "Pteromys volans",
          filter = c(province = "Uusimaa"),
          select = c("default_vars", "duration"),
          sample = TRUE, n = 5000, cache = FALSE, date_time_method = "none"
        )
      )
    )

    expect_s3_class(with_progress, "finbif_occ")

    expect_s3_class(
      finbif_occurrence("Vulpes vulpes", sample = TRUE, n = 1600, quiet = TRUE),
      "finbif_occ"
    )

    expect_s3_class(
      finbif_occurrence(
        select = "taxon_id", sample = TRUE, n = 3001, quiet = TRUE
      ),
      "finbif_occ"
    )

    expect_s3_class(
      finbif_occurrence(select = "-date_time"),
      "finbif_occ"
    )

  }

)

test_that(
  "can get a small random sample", {

    x <- finbif_occurrence(
      filter = c(n_total_records_max = 2000), aggregate = "records", n = 100
    )

    x <- subset(x, n_records < 3000, scientific_name_interpreted)

    expect_s3_class(
      finbif_occurrence(x[[1, 1]], sample = TRUE, n = 1001, quiet = TRUE),
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

    n <- 1100L

    fungi <- finbif_occurrence(
      filter = c(informal_groups = "Fungi and lichens"),
      select = to_native(
        "occurrenceID", "informalTaxonGroups", "taxonID", "vernacularName",
        "default_vars"
      ),
      n = n,
      quiet = TRUE
    )

    expect_output(print(fungi), "Records downloaded:")

    expect_output(
      print(fungi[c("scientific_name", "common_name")]), "A data"
    )

    expect_output(
      print(fungi[c(TRUE, rep_len(FALSE, n - 1L)), ]), "Records downloaded:"
    )

    expect_output(print(fungi[integer(0L), ]), "Records downloaded:")

    expect_output(
      print(fungi[1:10, c("scientific_name", "taxon_id")]), "A data"
    )

    options(finbif_cache_path = tempdir())

    expect_output(
      print(
        finbif_occurrence(
          select = c("default_vars", to_dwc("duration")), dwc = TRUE
        )
      ),
      "Records downloaded:"
    )

    expect_output(
      print(finbif_occurrence(aggregate = "species")), "Records downloaded:"
    )

    expect_output(print(finbif_occurrence()), "Records downloaded:")

    if (has_grd && !identical(.Platform$OS.type, "windows")) {

      expect_snapshot_file(
        save_svg(
          plot(fungi, axes = FALSE, xlab = NA, ylab = NA, panel.first = NULL)
        ),
        if (is_main_branch) "fungi.svg" else "fungi-dev.svg"
      )

    }

  }
)

test_that(
  "warns when taxa invalid", {

    skip_on_cran()

    expect_warning(finbif_occurrence("not a valid taxa"))

  }
)

test_that(
  "returns errors appropriately", {

    expect_error(
      finbif_occurrence("not a valid taxa", on_check_fail = "error")
    )

    expect_error(
      finbif_occurrence(filter = list(coordinates = list(c(60, 68), c(20, 30))))
    )

    expect_error(
      finbif_occurrence(filter = list(NULL), aggregate = "records", n = 2e5)
    )

    expect_error(
      finbif_occurrence("Birds", aggregate = "events")
    )

  }
)

suppressMessages(eject_cassette("finbif_occurrence"))

suppressMessages(insert_cassette("finbif_occurrence_has_media"))

test_that(
  "can process complex record variables", {

    skip_on_cran()

    has_media <- finbif_occurrence(
      filter = c(has_media = TRUE), select = c(media = "record_media_url"),
      sample = TRUE
    )

    url <- sample(unlist(has_media[["media"]]), 1L)

    expect_match(url, "^http")

  }
)

suppressMessages(eject_cassette("finbif_occurrence_has_media"))

suppressMessages(insert_cassette("finbif_occurrence_collection"))

test_that(
  "can process collection ids", {

    skip_on_cran()

    col_df <- finbif_occurrence(select = c("collection", "collection_id"))

    expect_s3_class(col_df, "finbif_occ")

    expect_false(identical(col_df[["collection"]], col_df[["collection_id"]]))

  }
)

suppressMessages(eject_cassette("finbif_occurrence_collection"))

test_that(
  "can make a multifilter request", {

    skip_on_cran()

    expect_s3_class(finbif_occurrence(filter = list(NULL, NULL)), "finbif_occ")

  }
)

suppressMessages(insert_cassette("finbif_occurrence_aggregate_events"))

test_that(
  "can aggregate by events", {

    skip_on_cran()

    expect_s3_class(
      finbif_occurrence(
        filter = c(location_id = "MNP.798"), aggregate = "events"
      ),
      "finbif_occ"
    )

  }

)

suppressMessages(eject_cassette("finbif_occurrence_aggregate_events"))

suppressMessages(insert_cassette("finbif_occurrence_aggregate_documents"))

test_that(
  "can aggregate by events", {

    skip_on_cran()

    expect_s3_class(
      finbif_occurrence(
        filter = c(location_id = "MNP.798"), aggregate = "documents"
      ),
      "finbif_occ"
    )

  }

)

suppressMessages(eject_cassette("finbif_occurrence_aggregate_documents"))

suppressMessages(insert_cassette("finbif_occurrence_date_time_ISO8601"))

test_that(
  "can create ISO8601 date strings", {

    skip_on_cran()

    expect_s3_class(
      finbif_occurrence(select = "date_time_ISO8601"),
      "finbif_occ"
    )

    expect_s3_class(
      finbif_occurrence(select = "eventDate", dwc = TRUE),
      "finbif_occ"
    )

  }

)

suppressMessages(eject_cassette("finbif_occurrence_date_time_ISO8601"))

suppressMessages(insert_cassette("finbif_occurrence_status"))

test_that(
  "can create occurrence status", {

    skip_on_cran()

    expect_s3_class(
      finbif_occurrence(select = "occurrence_status"), "finbif_occ"
    )

  }

)

suppressMessages(eject_cassette("finbif_occurrence_status"))

suppressMessages(insert_cassette("finbif_citation"))

test_that(
  "can create citation", {

    skip_on_cran()

    expect_s3_class(finbif_occurrence(select = "citation"), "finbif_occ")

  }

)

suppressMessages(eject_cassette("finbif_citation"))

suppressMessages(insert_cassette("finbif_get_all"))

test_that(
  "can get all records", {

    skip_on_cran()

    expect_s3_class(
      finbif_occurrence(
        filter = c(collection = "HR.778"), select = "record_id", n = -1
      ),
      "finbif_occ"
    )

  }

)

suppressMessages(eject_cassette("finbif_get_all"))
