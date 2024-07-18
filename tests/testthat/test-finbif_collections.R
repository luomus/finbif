test_that("fetching collections works", {

  skip_on_cran()

  op <- options()

  options(finbif_use_cache = FALSE, finbif_rate_limit = Inf)

  vcr::use_cassette("finbif_collections", {

    collections <- finbif_collections(
      has_children, NA, subcollections = FALSE, supercollections = TRUE
    )

    collections_error <- try(finbif_collections(1), silent = TRUE)

  })

  expect_snapshot(collections)

  expect_equal(
    collections_error[[1L]],
    paste(
      "Error in finbif_collections(1) :",
      "\n  Collections filter must be a logical vector\n"
    )
  )

  options(op)

})
