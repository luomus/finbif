test_that("fetching informal groups works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(finbif_cache_path = cache, finbif_rate_limit = Inf)

  vcr::use_cassette("finbif_informal_groups", {

    limit <- capture.output(finbif_informal_groups(limit = 1, locale = "ru"))

    bryophytes <- capture.output(finbif_informal_groups("Bryophytes"))

  })

  expect_equal(
    limit,
    c(
      "Algae",
      "  --Macro algae",
      "      --Brown algae and yellow green algae",
      "      --Green algae",
      "      --Red algae",
      "      --Stoneworts",
      "...148 more groups"
    )
  )

  expect_equal(
    bryophytes, c("Bryophytes", "  --Hornworts", "  --Liverworts", "  --Mosses")
  )

  options(finbif_cache_path = NULL)

  options(op)

})
