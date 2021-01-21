suppressMessages(insert_cassette("finbif_taxa"))

test_that(
  "returns valid data", {

    skip_on_cran()

    expect_s3_class(finbif_taxa("Parus major"), "finbif_api")

  }
)

suppressMessages(eject_cassette("finbif_taxa"))


suppressMessages(insert_cassette("taxon_name"))

test_that(
  "returns correct strings", {

    skip_on_cran()

    expect_identical(scientific_name("Otter"), "Lutra lutra")
    expect_identical(common_name("Bubo bubo", "se"), "lidnu")
    expect_identical(common_name("MX.279648"), NA_character_)

  }
)

suppressMessages(eject_cassette("taxon_name"))
