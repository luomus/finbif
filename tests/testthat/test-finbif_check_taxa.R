test_that("checking taxa works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(finbif_cache_path = cache, finbif_rate_limit = Inf)

  finbif_clear_cache()

  if (requireNamespace("vcr", quietly = TRUE)) {

    vcr::use_cassette("finbif_check_taxa", {

      cygnus_cygnus <- capture.output(
        print(finbif_check_taxa(list(species = "Cygnus cygnus", "not_a_taxa")))
      )

    })

    expect_identical(
      cygnus_cygnus,
      c(
        "[species: Cygnus cygnus] ID: MX.26280",
        "[         not_a_taxa   ] Not found"
      )
    )

  }

  options(finbif_cache_path = NULL)

  options(op)

})
