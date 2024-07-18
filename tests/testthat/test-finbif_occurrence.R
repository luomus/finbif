test_that("fetching occurrences works", {

  skip_on_cran()

  op <- options()

  options(
    finbif_cache_path = tempdir(),
    finbif_rate_limit = Inf,
    finbif_max_page_size = 5
  )

  vcr::use_cassette("finbif_occurrence", {

    count <- fb_occurrence(count_only = TRUE)

    birds <- capture.output(
      suppressMessages(
        finbif_occurrence(
          "Birds",
          select = c("default_vars", "-record_id"),
          filter = list(
            c(date_range_ymd = 2023),
            c(date_range_ymd = 2024)
          ),
          sample = TRUE
        )
      )
    )

    hr778 <- finbif_occurrence(
      taxa = "Red algae",
      filter = c(collection = "HR.778"), n = -1, quiet = TRUE
    )

  })

  expect_type(count, "integer")

  expect_snapshot(birds)

  expect_snapshot(hr778)

  options(op)

})
