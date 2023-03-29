test_that(
  "sysdata are accessible", {

    skip_on_cran()

    expect_s3_class(has_value(), "data.frame")

    expect_s3_class(threatened_status(), "data.frame")

    expect_s3_class(informal_groups_reported(), "data.frame")

    expect_s3_class(finnish_occurrence_status_neg(), "data.frame")

    expect_s3_class(superrecord_basis(), "data.frame")

    expect_s3_class(quality_issues(), "data.frame")

    vcr::use_cassette("collection_quality", {

      cq <- collection_quality()

    })

    expect_s3_class(cq, "data.frame")

    expect_s3_class(record_reliability(), "data.frame")

    expect_s3_class(complete_list_type(), "data.frame")

    vcr::use_cassette("location_tag", {

      lt <- location_tag()

    })

    expect_s3_class(lt, "data.frame")

    vcr::use_cassette("atlas_code", {

      aco <- atlas_code()

    })

    expect_s3_class(aco, "data.frame")

    vcr::use_cassette("atlas_class", {

      acl <- atlas_class()

    })

    expect_s3_class(acl, "data.frame")

  }
)
