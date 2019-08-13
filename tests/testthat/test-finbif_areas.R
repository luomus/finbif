context("Checking FinBIF area functions")

test_that(
  "returns valid data", {
    expect_s3_class(finbif_countries(), "data.frame")
    expect_s3_class(finbif_provinces(), "data.frame")
    expect_s3_class(finbif_municipalities(), "data.frame")
    expect_s3_class(finbif_bird_assoc_areas(), "data.frame")
  }
)
