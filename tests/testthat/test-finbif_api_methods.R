context("FinBIF api methods")

test_that("are working", {
  vcr::use_cassette("finbif_records", {
    resp_list_n1 <- finbif_records(n = 1)
    resp_list_n101 <- finbif_records(n = 101)
  }, preserve_exact_body_bytes = TRUE)

  expect_output(print(resp_list_n101[[1]]), "FinBIF")
  expect_output(resp_list_n1_2 <- print(resp_list_n1), "FinBIF")
  expect_identical(resp_list_n1_2, resp_list_n1)
  expect_s3_class(as.data.frame(resp_list_n101), "data.frame")
})
