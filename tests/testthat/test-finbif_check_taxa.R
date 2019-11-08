context("FinBIF taxa checking")

vcr::use_cassette(
  "finbif_check_taxa", {

    test_that(
      "returns valid data when taxa exist", {

        skip_on_cran()

        sp_true <- finbif_check_taxa(list(species = "Parus major"))

        expect_type(sp_true, "list")

        expect_output(print(sp_true), "species: Parus major")

      }
    )

    test_that(
      "returns valid data when taxa don't exist", {

        skip_on_cran()

        expect_type(finbif_check_taxa(list(species = "Parus najor")), "list")

        expect_type(finbif_check_taxa(list(genus = "Parus major")), "list")

      }
    )

  },
  preserve_exact_body_bytes = TRUE
)
