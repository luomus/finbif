test_that(
  "sysdata are accessible", {

    skip_on_cran()

    expect_s3_class(sysdata("has_value"), "data.frame")

    vcr::use_cassette("threatened_status", {

      ts <- sysdata("threatened_status")

    })

    expect_s3_class(ts, "data.frame")

    expect_s3_class(sysdata("informal_groups_reported"), "data.frame")

    expect_s3_class(sysdata("finnish_occurrence_status_neg"), "data.frame")

    expect_s3_class(sysdata("superrecord_basis"), "data.frame")

    expect_s3_class(sysdata("quality_issues"), "data.frame")

    vcr::use_cassette("collection_quality", {

      cq <- sysdata("collection_quality")

    })

    expect_s3_class(cq, "data.frame")

    expect_s3_class(sysdata("record_reliability"), "data.frame")

    expect_s3_class(sysdata("complete_list_type"), "data.frame")

    vcr::use_cassette("location_tag", {

      lt <- sysdata("location_tag")

    })

    expect_s3_class(lt, "data.frame")

    vcr::use_cassette("atlas_code", {

      aco <- sysdata("atlas_code")

    })

    expect_s3_class(aco, "data.frame")

    vcr::use_cassette("atlas_class", {

      acl <- sysdata("atlas_class")

    })

    expect_s3_class(acl, "data.frame")

    expect_s3_class(
      sysdata("primary_secondary_habitat")[["habitat_types"]], "data.frame"
    )

    expect_s3_class(sysdata("orig_taxon_rank"), "data.frame")

  }
)
