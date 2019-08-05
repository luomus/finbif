context("Checking FinBIF taxonomic ranks")

test_that(
  "returns valid data", {
    expect_type(finbif_taxon_ranks(), "character")
  }
)
