test_that("checking taxa works", {

  skip_on_cran()

  op <- options()

  options(finbif_use_cache = FALSE, finbif_rate_limit = Inf)

  vcr::use_cassette("finbif_check_taxa", {

    cygnus_cygnus <- finbif_check_taxa(c(species = "Cygnus cygnus"))

  })

  expect_equal(
    cygnus_cygnus,
    structure(
      list(species = c("Cygnus cygnus" = "MX.26280")),
      class = c("list", "finbif_taxa_list")
    )
  )

  options(op)

})
