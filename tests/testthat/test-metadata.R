test_that(
  "return valid data", {

    skip_on_cran()

    expect_s3_class(finbif_metadata(), "data.frame")

    options(finbif_locale = "ru")

    expect_s3_class(finbif_metadata("red_list"), "data.frame")

    vcr::use_cassette("country_metadata", {

      cmd <- finbif_metadata("country")

    })

    expect_s3_class(cmd, "data.frame")

    vcr::use_cassette("region_metadata", {

      rmd <- finbif_metadata("region")

    })

    expect_s3_class(rmd, "data.frame")

    vcr::use_cassette("bio_province_metadata", {

      bpmd <- finbif_metadata("bio_province")

    })

    expect_s3_class(bpmd, "data.frame")

    vcr::use_cassette("municipality_metadata", {

      mmd <- finbif_metadata("municipality")

    })

    expect_s3_class(mmd, "data.frame")

    vcr::use_cassette("bird_assoc_area_metadata", {

      baamd <- finbif_metadata("bird_assoc_area")

    })

    expect_s3_class(baamd, "data.frame")

    vcr::use_cassette("finnish_occurrence_status_metadata", {

      fosmd <- finbif_metadata("finnish_occurrence_status")

    })

    expect_s3_class(fosmd, "data.frame")

    vcr::use_cassette("source_metadata", {

      smd <- finbif_metadata("source")

    })

    expect_s3_class(smd, "data.frame")

    vcr::use_cassette("record_basis_metadata", {

      rbmd <- finbif_metadata("record_basis")

    })

    expect_s3_class(rbmd, "data.frame")

    expect_s3_class(finbif_metadata("sex_category"), "data.frame")

    expect_s3_class(finbif_metadata("restriction_reason"), "data.frame")

    vcr::use_cassette("restriction_level_metadata", {

      rlmd <- finbif_metadata("restriction_level")

    })

    expect_s3_class(rlmd, "data.frame")

    vcr::use_cassette("life_stage_metadata", {

      lsmd <- finbif_metadata("life_stage")

    })

    expect_s3_class(lsmd, "data.frame")

    vcr::use_cassette("taxon_rank_metadata", {

      trmd <- finbif_metadata("taxon_rank")

    })

    expect_s3_class(trmd, "data.frame")

    vcr::use_cassette("regulatory_status_metadata", {

      rsmd <- finbif_metadata("regulatory_status")

    })

    expect_s3_class(rsmd, "data.frame")

    expect_s3_class(finbif_metadata("habitat_type"), "data.frame")

    expect_s3_class(finbif_metadata("habitat_qualifier"), "data.frame")

    capture.output(informal_groups <- finbif_informal_groups())

    expect_type(informal_groups, "character")

    expect_type(finbif_informal_groups("Algae", quiet = TRUE), "character")

  }
)

test_that(
  "print method works", {

    skip_on_cran()

    vcr::use_cassette("taxon_rank_metadata", {

      trmd <- finbif_metadata("taxon_rank")

    })

    expect_output(print(trmd), "rank")

  }

)

test_that(
  "returns errors appropriately", {
    expect_error(finbif_metadata("notmetadata"))
  }
)
