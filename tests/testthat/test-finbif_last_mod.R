suppressMessages(insert_cassette("finbif_last_mod"))

test_that(
  "returns a valid date", {

    skip_on_cran()

    expect_s3_class(
      finbif_last_mod("Cygnus cygnus", filter = c(country = "Finland")),
      "Date"
    )

  }
)

suppressMessages(eject_cassette("finbif_last_mod"))
