test_that("searching for taxa works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(
    finbif_cache_path = cache,
    finbif_rate_limit = Inf,
    finbif_email = "noreply@laji.fi"
  )

  finbif_clear_cache()

  if (requireNamespace("vcr", quietly = TRUE)) {

    vcr::use_cassette("finbif_taxa", {

      bubo_bubo <- common_name("Bubo bubo")

      otter <- scientific_name("Otter")

      otter_id <- taxon_id("Otter")

    })

    expect_equal(bubo_bubo, "Eurasian Eagle-owl")

    expect_equal(otter, "Lutra lutra")

    expect_equal(otter_id, "MX.47169")

  }

  options(finbif_cache_path = NULL, finbif_email = NULL)

  options(op)

})

test_that("invalid json triggers error", {

  skip_on_cran()

  op <- options()

  f <- tempfile()

  if (
    requireNamespace("callr", quietly = TRUE) &&
    requireNamespace("webfakes", quietly = TRUE)
  ) {

    bg <- callr::r_bg(
      function(file, version) {

        app <- webfakes::new_app()

        app[["get"]](
          sprintf("/%s/taxa/search", version),
          function(req, res) {
            res[["send_json"]](text = "'invalid json]")
          }
        )

        web <- webfakes::local_app_process(app)

        cat(c(web[["url"]](), "."), file = file, sep = "\n")

        Sys.sleep(60L)

      },
      list(file = f, version = getOption("finbif_api_version"))
    )

    while (!file.exists(f) || length(url <- readLines(f, warn = FALSE)) < 2L) {}

    options(finbif_api_url = sub("/$", "", url[[1L]]), finbif_rate_limit = Inf)

    expect_error(finbif_taxa("Invalid JSON"), "API response parsing failed")

  }

  options(op)

})
