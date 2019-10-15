context("Checking FinBIF internal data functions")

test_that(
  "return valid data", {

    expect_s3_class(finbif_metadata(), "data.frame")

    expect_s3_class(finbif_metadata("admin_status"), "data.frame")

    expect_s3_class(finbif_metadata("red_list"), "data.frame")

    expect_s3_class(finbif_metadata("habitat_types"), "data.frame")

    expect_s3_class(finbif_metadata("habitat_qualifiers"), "data.frame")

    expect_s3_class(finbif_metadata("countries"), "data.frame")

    expect_s3_class(finbif_metadata("provinces"), "data.frame")

    expect_s3_class(finbif_metadata("municipalities"), "data.frame")

    expect_s3_class(finbif_metadata("bird_assoc_areas"), "data.frame")

    expect_s3_class(finbif_metadata("finnish_occurrence_status"), "data.frame")

    expect_s3_class(finbif_metadata("sources"), "data.frame")

    expect_s3_class(finbif_metadata("record_basis"), "data.frame")

    expect_s3_class(finbif_metadata("sex_categories"), "data.frame")

    expect_s3_class(finbif_metadata("restriction_reasons"), "data.frame")

    expect_s3_class(finbif_metadata("restriction_levels"), "data.frame")

    expect_s3_class(finbif_metadata("life_stages"), "data.frame")

    expect_s3_class(finbif_metadata("taxon_ranks"), "data.frame")

    expect_type(finbif_informal_groups(), "character")

    expect_type(finbif_informal_groups("Algae"), "character")

  }
)


test_that(
  "returns errors appropriately", expect_error(finbif_metadata("notmetdata"))
)
