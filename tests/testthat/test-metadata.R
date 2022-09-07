test_that(
  "return valid data", {

    expect_s3_class(finbif_metadata(), "data.frame")

    expect_s3_class(finbif_metadata("regulatory_statuses"), "data.frame")

    expect_s3_class(finbif_metadata("red_list"), "data.frame")

    expect_s3_class(finbif_metadata("habitat_type"), "data.frame")

    expect_s3_class(finbif_metadata("habitat_qualifier"), "data.frame")

    expect_s3_class(finbif_metadata("country"), "data.frame")

    expect_s3_class(finbif_metadata("province"), "data.frame")

    expect_s3_class(finbif_metadata("municipality"), "data.frame")

    expect_s3_class(finbif_metadata("bird_assoc_area"), "data.frame")

    expect_s3_class(finbif_metadata("finnish_occurrence_status"), "data.frame")

    expect_s3_class(finbif_metadata("source"), "data.frame")

    expect_s3_class(finbif_metadata("record_basis"), "data.frame")

    expect_s3_class(finbif_metadata("sex_category"), "data.frame")

    expect_s3_class(finbif_metadata("restriction_reason"), "data.frame")

    expect_s3_class(finbif_metadata("restriction_level"), "data.frame")

    expect_s3_class(finbif_metadata("life_stage"), "data.frame")

    expect_s3_class(finbif_metadata("taxon_rank"), "data.frame")

    capture.output(informal_groups <- finbif_informal_groups())

    expect_type(informal_groups, "character")

    expect_type(finbif_informal_groups("Algae", quiet = TRUE), "character")

  }
)

test_that(
  "print method works", {
    expect_output(print(finbif_metadata("taxon_rank")), "rank")
  }
)

test_that(
  "returns errors appropriately", {
    expect_error(finbif_metadata("notmetdata"))
  }
)
