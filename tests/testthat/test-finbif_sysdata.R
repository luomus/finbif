context("Checking FinBIF internal data functions")

test_that(
  "return valid data", {

    expect_s3_class(finbif_admin_status(), "data.frame")

    expect_s3_class(finbif_red_list(), "data.frame")

    expect_s3_class(finbif_habitat_types(), "data.frame")

    expect_s3_class(finbif_habitat_qualifiers(), "data.frame")

    expect_s3_class(finbif_countries(), "data.frame")

    expect_s3_class(finbif_provinces(), "data.frame")

    expect_s3_class(finbif_municipalities(), "data.frame")

    expect_s3_class(finbif_bird_assoc_areas(), "data.frame")

    expect_s3_class(finbif_finnish_occurrence_status(), "data.frame")

    expect_s3_class(finbif_sources(), "data.frame")

    expect_s3_class(finbif_record_basis(), "data.frame")

    expect_s3_class(finbif_sex_categories(), "data.frame")

    expect_s3_class(finbif_restriction_reasons(), "data.frame")

    expect_s3_class(finbif_restriction_levels(), "data.frame")

    expect_type(finbif_life_stages(), "character")

    expect_type(finbif_informal_groups(), "character")

    expect_type(finbif_informal_groups("Algae"), "character")

    expect_type(finbif_taxon_ranks(), "character")

  }
)
