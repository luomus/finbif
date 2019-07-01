context('Downloading FinBIF occurrence data')

test_that("returns valid data", {
  vcr::use_cassette("finbif_occurrence", {
    resp<- finbif_occurrence(list(species = "Parus major"))
  }, preserve_exact_body_bytes = TRUE)

  expect_s3_class(resp, "data.frame")
})
