context("Checking FinBIF informal groups")

test_that(
  "returns valid data", {
    expect_type(finbif_informal_groups(), "character")
    expect_type(finbif_informal_groups("Algae"), "character")
  }
)
