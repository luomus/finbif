test_that(
  "sysdata are accessible", {

    skip_on_cran()

    expect_s3_class(has_value(), "data.frame")

    vcr::use_cassette("threatened_status", {

      ts <- threatened_status()

    })

    expect_s3_class(ts, "data.frame")

    expect_s3_class(informal_groups_reported(), "data.frame")

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
