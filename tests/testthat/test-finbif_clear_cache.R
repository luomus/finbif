context("Checking clearing FinBIF cache")

test_that(
  "works", {

    expect_null(finbif_clear_cache())

  }
)
