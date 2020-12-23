test_that(
  "works", {
    options("finbif_use_cache" = NULL)
    finbif:::.onLoad()
    expect_type(getOption("finbif_use_cache"), "logical")
  }
)
