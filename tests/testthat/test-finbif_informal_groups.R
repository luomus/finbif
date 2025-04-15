test_that("fetching informal groups works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(finbif_cache_path = cache, finbif_rate_limit = Inf)

  if (requireNamespace("vcr", quietly = TRUE)) {

    vcr::use_cassette("finbif_informal_groups", {

      limit <- capture.output(finbif_informal_groups(limit = 1, locale = "ru"))

      bryophytes <- capture.output(finbif_informal_groups("Bryophytes"))

    })

    expect_equal(
      limit,
      c(
        "Birds",
        "  --Birds of prey and owls",
        "      --Owls",
        "      --Birds of prey",
        "  --Waterbirds",
        "...149 more groups"
      )
    )

    expect_equal(
      bryophytes,
      c("Bryophytes", "  --Mosses", "  --Liverworts", "  --Hornworts")
    )

  }

  options(finbif_cache_path = NULL)

  options(op)

})
