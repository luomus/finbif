test_that(
  "sysdata are accessible", {

    expect_s3_class(has_value(), "data.frame")

    expect_s3_class(threatened_status(), "data.frame")

    expect_s3_class(informal_groups_reported(), "data.frame")

    expect_type(primary_secondary_habitat(), "list")

    expect_s3_class(orig_taxon_rank(), "data.frame")

    expect_s3_class(finnish_occurrence_status_neg(), "data.frame")

    expect_s3_class(superrecord_basis(), "data.frame")

    expect_s3_class(quality_issues(), "data.frame")

    expect_s3_class(collection_quality(), "data.frame")

    expect_s3_class(record_reliability(), "data.frame")

    expect_s3_class(complete_list_type(), "data.frame")

    expect_s3_class(location_tag(), "data.frame")

    expect_s3_class(atlas_code(), "data.frame")

    expect_s3_class(atlas_class(), "data.frame")

  }
)
