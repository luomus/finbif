test_that(
  "return valid data", {

    skip_on_cran()

    expect_s3_class(finbif_metadata(), "data.frame")

    options(finbif_locale = "se")

    vcr::use_cassette("red_list_metadata", {

      rlmd <- finbif_metadata("red_list")

    })

    expect_s3_class(rlmd, "data.frame")

    vcr::use_cassette("country_metadata", {

      cmd <- finbif_metadata("country")

    })

    expect_s3_class(cmd, "data.frame")

    vcr::use_cassette("region_metadata", {

      rmd <- finbif_metadata("region")

    })

    expect_s3_class(rmd, "data.frame")

    expect_s3_class(finbif_metadata("bio_province"), "data.frame")

    expect_s3_class(finbif_metadata("municipality"), "data.frame")

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

    expect_s3_class(finbif_metadata("record_basis"), "data.frame")

    expect_s3_class(finbif_metadata("sex_category"), "data.frame")

    expect_s3_class(finbif_metadata("restriction_reason"), "data.frame")

    vcr::use_cassette("restriction_level_metadata", {

      rlmd <- finbif_metadata("restriction_level")

    })

    expect_s3_class(rlmd, "data.frame")

    expect_s3_class(finbif_metadata("life_stage"), "data.frame")

    vcr::use_cassette("taxon_rank_metadata", {

      trmd <- finbif_metadata("taxon_rank")

    })

    expect_s3_class(trmd, "data.frame")

    vcr::use_cassette("regulatory_status_metadata", {

      rsmd <- finbif_metadata("regulatory_status")

    })

    expect_s3_class(rsmd, "data.frame")

    vcr::use_cassette("habitat_type_metadata", {

      htmd <- finbif_metadata("habitat_type")

    })

    expect_s3_class(htmd, "data.frame")

    expect_s3_class(finbif_metadata("habitat_qualifier"), "data.frame")

    vcr::use_cassette("informal_group_metadata", {

      capture.output(informal_groups <- finbif_informal_groups())

    })

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

    expect_output(print(trmd), "name")

  }

)

test_that(
  "returns errors appropriately", {

    expect_error(finbif_metadata("notmetadata"))

  }
)
