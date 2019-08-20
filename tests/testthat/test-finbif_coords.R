context("Converting coordinates to FinBIF format")

test_that(
  "returns valid data", {

     expect_type(finbif_coords(67, 32, "kkj"), "character")
     expect_type(finbif_coords(67, 32, "kkj", 1), "character")

  }
)
