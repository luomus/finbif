test_that("checking taxa works", {

  skip_on_cran()

  op <- options()

  cache <- tempfile()

  dir.create(cache)

  options(finbif_cache_path = cache, finbif_rate_limit = Inf)

  finbif_clear_cache()

  if (requireNamespace("vcr", quietly = TRUE)) {

    vcr::use_cassette("finbif_check_taxa", {

      capture.output(
        cygnus_cygnus <- print(
          finbif_check_taxa(list(species = "Cygnus cygnus", "not_a_taxa"))
        )
      )

    })

    expect_equal(
      cygnus_cygnus,
      structure(
        list(
          species = c("Cygnus cygnus" = "MX.26280"),
          c(not_a_taxa = NA_character_)
        ),
        class = c("list", "finbif_taxa_list")
      )
    )

  }

  options(finbif_cache_path = NULL)

  options(op)

})
