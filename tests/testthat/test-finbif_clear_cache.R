context("Checking FinBIF clear cache")

test_that(
  "works", {
    expect_identical(finbif_clear_cache(), 0L)
  }
)
