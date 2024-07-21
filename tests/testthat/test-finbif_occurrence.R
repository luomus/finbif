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

  vcr::use_cassette("finbif_occurrence", {

    count <- fb_occurrence(count_only = TRUE)

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
            "informal_groups"
          ),
          filter = list(
            c(date_range_ymd = 2023),
            c(date_range_ymd = 2024)
          ),
          facts = c(pairs = "MY.pairCount"),
          sample = TRUE,
          unlist = TRUE,
          drop_na = TRUE
        )
      )
    )

    hr778 <- finbif_occurrence(
      taxa = "Red algae",
      filter = c(collection = "HR.778"), n = -1, quiet = TRUE
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

  options(finbif_cache_path = NULL)

  options(op)

})
