context("Checking option setting")

test_that(
  "works", {
    finbif:::.onLoad()
    expect_type(getOption("finbif_use_cache"), "logical")
  }
)
