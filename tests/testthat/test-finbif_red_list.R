context("Checking FinBIF red list status")

test_that(
  "returns valid data", {
    expect_s3_class(finbif_red_list(), "data.frame")
  }
)
