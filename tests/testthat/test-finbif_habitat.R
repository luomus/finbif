context("Checking FinBIF habitat types and qualifiers")

test_that(
  "returns valid data", {
    expect_s3_class(finbif_habitat_types(), "data.frame")
    expect_s3_class(finbif_habitat_qualifiers(), "data.frame")
  }
)
