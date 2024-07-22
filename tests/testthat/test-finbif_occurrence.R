test_that("fetching occurrences works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(
    finbif_cache_path = cache,
    finbif_rate_limit = Inf,
    finbif_max_page_size = 5
  )

  finbif_clear_cache()

  if (requireNamespace("vcr", quietly = TRUE)) {

    vcr::use_cassette("finbif_occurrence", {

      count <- finbif_occurrence(
        "Plants", filter = c(date_range_ymd = 2023), count_only = TRUE
      )

      birds <- capture.output(
        suppressMessages(
          finbif_occurrence(
            "Birds",
            select = c(
              "default_vars",
              "-record_id",
              "duration",
              "date_time_ISO8601",
              "collection",
              "primary_habitat",
              "epsg",
              "occurrence_status",
              "citation",
              "collection_code",
              "red_list_status",
              "region",
              "informal_groups",
              "common_name",
              "restriction_reason",
              "atlas_code",
              "atlas_class"
            ),
            filter = list(
              c(date_range_ymd = 2023),
              c(
                date_range_ymd = 2024,
                informal_groups_reported = "Birds",
                primary_secondary_habitat = "M",
                record_reliability = "reliable",
                complete_list_type = "incomplete"
              )
            ),
            facts = c(pairs = "MY.pairCount"),
            sample = TRUE,
            unlist = TRUE,
            drop_na = TRUE,
            tzone = "Etc/UTC"
          )
        )
      )

      hr778 <- finbif_occurrence(
        taxa = "Red algae",
        filter = c(collection = "HR.778"),
        n = -1,
        quiet = TRUE
      )

      hr778_no_records <- finbif_occurrence(
        taxa = "Birds",
        filter = list(
          set1 = c(collection = "HR.778"),
          set2 = c(collection = "HR.778")
        ),
        select = "municipality",
        filter_col = "set",
        quiet = TRUE
      )

      options(finbif_max_page_size = 20, finbif_use_async = FALSE)

      plants <- finbif_occurrence(
        "Plants",
        select = c(
          "threatened_status",
          "orig_taxon_rank"
        ),
        filter = list(
          date_range_ymd = 2023,
          has_value = "record_id",
          quality_issues = "without_issues",
          finnish_occurrence_status_neg = "extinct",
          subset = c(1, floor(count / 50))
        ),
        sample = TRUE,
        n = "all",
        quiet = TRUE
      )

      options(finbif_max_page_size = 5)

      no_filter_error <- try(
        finbif_occurrence(
          filter = list(
            not_filter = TRUE,
            primary_habitat = list(M = "V"),
            collection = finbif_collections(
              taxonomic_coverage == "Coleoptera",
              supercollections = TRUE,
              nmin = NA
            )
          ),
          n = 3e6
        ),
        silent = TRUE
      )

      coord_filter_error <- try(
        finbif_occurrence(
          filter = list(
            primary_habitat = "M", coordinates = list(c(60, 68), c(20, 30))
          ),
          n = 0
        ),
        silent = TRUE
      )

      invalid_taxa_error <- try(
        finbif_occurrence("Algae", on_check_fail = "error"), silent = TRUE
      )

      options(warn = 2)

      invalid_taxa_warn <- try(
        finbif_occurrence("Algae", on_check_fail = "warn"), silent = TRUE
      )

      options(warn = 0)

    })

    expect_type(count, "integer")

    expect_snapshot(birds)

    expect_snapshot(hr778)

    expect_snapshot(hr778_no_records)

    expect_snapshot(plants)

    expect_match(no_filter_error, "Invalid name in filter names")

    expect_match(no_filter_error, "Cannot download more than")

    expect_match(coord_filter_error, "Invalid coordinates: system not specified")

    expect_match(coord_filter_error, "Cannot request less than 1 record")

    expect_equal(
      invalid_taxa_error[[1L]],
      paste(
        "Error : Cannot find the following taxa in the FinBIF taxonomy.\nPlease",
        "check you are using accepted names and not synonyms or\nother names for",
        "the taxa you are selecting:\n\nAlgae\n"
      )
    )

    expect_equal(
      invalid_taxa_warn[[1L]],
      paste(
        "Error : (converted from warning) Cannot find the following taxa in the",
        "FinBIF taxonomy.\nPlease check you are using accepted names and not",
        "synonyms or\nother names for the taxa you are selecting:\n\nAlgae\n"
      )
    )

    vcr::use_cassette("finbif_occurrence_print", {

      capture.output(
        occ_print <- suppressMessages(
          print(finbif_occurrence(select = "informal_groups", n = 11))
        )
      )

    })

    expect_snapshot(occ_print)

    options(finbif_warehouse_query = "xxxx/")

    vcr::use_cassette("finbif_occurrence_api_error", {

      api_error <- try(finbif_occurrence(), silent = TRUE)

    })

    expect_match(api_error, "API request failed")

  }

  finbif_clear_cache()

  options(finbif_cache_path = NULL)

  options(op)

})

test_that("fetching occurrences with date filters works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(
    finbif_cache_path = cache,
    finbif_rate_limit = Inf,
    finbif_max_page_size = 5
  )

  finbif_clear_cache()

  if (requireNamespace("vcr", quietly = TRUE)) {

    vcr::use_cassette("finbif_occurrence_dates", {

      date_filters <- finbif_occurrence(
        filter = list(
          list(
            date_range_ymd = structure(
              list(
                start = as.Date("2001-05-01"),
                .Data = as.difftime(609, units = "days")
                ),
              class = "Interval"
            )
          ),
          list(date_range_ymd = c("2001", "2002-06-30")),
          list(date_range_ymd = c("2001-01-01", "2002")),
          list(date_range_ymd = as.Date("2001-05-01")),
          list(date_range_ymd = 2001:2002),
          list(date_range_ym = c("2001-01", "2002-12")),
          list(date_range_md = "01-01")
        ),
        n = 5
      )

      date_error <- try(
        finbif_occurrence(filter = c(date_range_ymd = "not_a_date")),
        silent = TRUE
      )

    })

    expect_snapshot(date_filters)

    expect_equal(
      date_error[[1]],
      "Error : 1 error occurred:\n  - Can't parse one or more specified dates\n"
    )

  }

  finbif_clear_cache()

  options(finbif_cache_path = NULL)

  options(op)

})

test_that("fetching aggregated occurrences works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(
    finbif_cache_path = cache,
    finbif_rate_limit = Inf,
    finbif_max_page_size = 5
  )

  finbif_clear_cache()

  if (requireNamespace("vcr", quietly = TRUE)) {

    vcr::use_cassette("finbif_occurrence_aggregate", {

      record_basis_count <- finbif_occurrence(
        select = "record_basis",
        filter = list(coordinates = list(c(60.4, 61), c(22, 22.5), "wgs84", 1)),
        aggregate = "records",
        count_only = TRUE
      )

      record_basis_aggregate <- finbif_occurrence(
        select = "basisOfRecord",
        filter = c(location_tag = "Farmland"),
        aggregate = c("records", "species"),
        n = 5,
        exclude_na = TRUE,
        dwc = TRUE
      )

      aggregate_error <- try(
        finbif_occurrence(
          "Birds",
          select = "-event_id",
          aggregate = c("events", "species"),
          check_taxa = FALSE
        ),
        silent = TRUE
      )

    })

    expect_type(record_basis_count, "integer")

    expect_snapshot(record_basis_aggregate)

    expect_match(
      aggregate_error,
      "Chosen aggregation cannot by combined with other aggregations"
    )

    expect_match(
      aggregate_error,
      "Cannot use current aggregation and filter by taxon"
    )

  }

  finbif_clear_cache()

  options(finbif_cache_path = NULL)

  options(op)

})
