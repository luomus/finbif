context("Checking FinBIF countries")

test_that(
  "returns valid data", {
    expect_s3_class(finbif_countries(), "data.frame")
  }
)
