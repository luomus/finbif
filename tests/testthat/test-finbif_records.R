context('Downloading FinBIF records')

test_that("returns valid data", {
  vcr::use_cassette("finbif_records", {
    resp_list <- finbif_records(n = 1)
  }, preserve_exact_body_bytes = TRUE)

  expect_s3_class(resp_list, "finbif_api")

})
