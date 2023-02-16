test_that(
  "works", {

    expect_null(finbif_clear_cache())

    options(finbif_cache_path = getwd())

    expect_null(finbif_clear_cache())

    options(finbif_cache_path = NULL)

  }
)
