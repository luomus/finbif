context("Date formattting functions")

vcr::use_cassette(
  "finbif_dates", {

    test_that(
      "return valid data", {

        records <-
          finbif_records(filter = list(date_range_ymd = c("2001", "2008")))

        expect_output(print(records), "FinBIF")

      }
    )

  },
  preserve_exact_body_bytes = TRUE
)


test_that(
  "returns errors appropriately", {

    expect_condition(finbif_dates("date_range_ymd"))
    expect_condition(finbif_dates("date_range_ymd", "this is not a date"))

  }
)

test_that(
  "correctly convert dates", {

    f <- "date_range_ymd"
    expect_identical(finbif_dates(f, "2001"), "2001")
    expect_identical(finbif_dates(f, 2001), "2001")
    expect_identical(finbif_dates(f, 2001L), "2001")
    expect_identical(finbif_dates(f, end = "2001"), "2001")
    expect_identical(finbif_dates(f, end = 2001), "2001")
    expect_identical(finbif_dates(f, end = 2001L), "2001")

    expect_identical(finbif_dates(f, "2001-05"), "2001-05")
    expect_identical(finbif_dates(f, end = "2001-05"), "2001-05")

    expect_identical(finbif_dates(f, "2001-05-01"), "2001-05-01")
    expect_identical(finbif_dates(f, end = "2001-05-01"), "2001-05-01")

    expect_identical(
      finbif_dates(f, lubridate::as_date("2001-05-01")), "2001-05-01"
    )
    expect_identical(
      finbif_dates(f, end = lubridate::as_date("2001-05-01")), "2001-05-01"
    )

    expect_identical(finbif_dates(f, 2001, 2004), "2001/2004")
    expect_identical(finbif_dates(f, "2001", 2004), "2001/2004")
    expect_identical(finbif_dates(f, 2001L, 2004), "2001/2004")
    expect_identical(finbif_dates(f, 2001, "2004"), "2001/2004")

    expect_identical(finbif_dates(f, "2001-05", "2002-06"), "2001-05/2002-06")

    expect_identical(finbif_dates(f, "2001", "2002-06"), "2001-01/2002-06")
    expect_identical(finbif_dates(f, "2001-05", "2003"), "2001-05/2003-12")

    expect_identical(
      finbif_dates(f, "2001-05-01", "2002-06-11"), "2001-05-01/2002-06-11"
    )
    expect_identical(
      finbif_dates(f, "2001", "2002-06-11"), "2001-01-01/2002-06-11"
    )
    expect_identical(
      finbif_dates(f, "2001-02", "2002-06-11"), "2001-02-01/2002-06-11"
    )
    expect_identical(
      finbif_dates(f, "2001-05-01", "2002-06"), "2001-05-01/2002-06-30"
    )
    expect_identical(
      finbif_dates(f, "2001-05-01", "2002"), "2001-05-01/2002-12-31"
    )
    expect_identical(
      finbif_dates(f, lubridate::interval("2001-05-01", "2002-12-31")),
      "2001-05-01/2002-12-31"
    )

    f <- "date_range_ym"
    expect_identical(finbif_dates(f, "2001"), "2001")
    expect_identical(finbif_dates(f, 2001), "2001")
    expect_identical(finbif_dates(f, 2001L), "2001")
    expect_identical(finbif_dates(f, end = "2001"), "2001")
    expect_identical(finbif_dates(f, end = 2001), "2001")
    expect_identical(finbif_dates(f, end = 2001L), "2001")

    expect_identical(finbif_dates(f, "2001-05"), "2001-05")
    expect_identical(finbif_dates(f, end = "2001-05"), "2001-05")

    expect_identical(finbif_dates(f, "2001-05-01"), "2001-05")
    expect_identical(finbif_dates(f, end = "2001-05-01"), "2001-05")

    expect_identical(
      finbif_dates(f, lubridate::as_date("2001-05-01")), "2001-05"
    )
    expect_identical(
      finbif_dates(f, end = lubridate::as_date("2001-05-01")), "2001-05"
    )

    expect_identical(finbif_dates(f, 2001, 2004), "2001/2004")
    expect_identical(finbif_dates(f, "2001", 2004), "2001/2004")
    expect_identical(finbif_dates(f, 2001L, 2004), "2001/2004")
    expect_identical(finbif_dates(f, 2001, "2004"), "2001/2004")

    expect_identical(finbif_dates(f, "2001-05", "2002-06"), "2001-05/2002-06")

    expect_identical(finbif_dates(f, "2001", "2002-06"), "2001-01/2002-06")
    expect_identical(finbif_dates(f, "2001-05", "2003"), "2001-05/2003-12")

    expect_identical(
      finbif_dates(f, "2001-05-01", "2002-06-11"), "2001-05/2002-06"
    )
    expect_identical(finbif_dates(f, "2001", "2002-06-11"), "2001-01/2002-06")
    expect_identical(
      finbif_dates(f, "2001-02", "2002-06-11"), "2001-02/2002-06"
    )
    expect_identical(
      finbif_dates(f, "2001-05-01", "2002-06"), "2001-05/2002-06"
    )
    expect_identical(finbif_dates(f, "2001-05-01", "2002"), "2001-05/2002-12")

    expect_identical(finbif_dates("date_range_d", 1, 50), "1/50")
    expect_identical(finbif_dates("date_range_md", "05-03", "06-01"), "503/601")
    expect_identical(finbif_dates("last_import_date_min", 2001), "2001")
    expect_identical(finbif_dates("last_import_date_max", 2001), "2001")
    expect_identical(finbif_dates("first_import_date_min", 2001), "2001")
    expect_identical(finbif_dates("first_import_date_max", 2001), "2001")
  }
)