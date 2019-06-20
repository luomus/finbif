context("FinBIF api methods")

test_that("are working", {
  vcr::use_cassette("finbif_api_get", {
    resp_list <- finbif_api_get(
      path = "v0/warehouse/query/list",
      query = list(page = 1, pageSize = 1, selected = "unit.unitId")
    )
  }, preserve_exact_body_bytes = TRUE)
  expect_output(resp_list_2 <- print(resp_list), "FinBIF")
  expect_identical(resp_list_2, resp_list)
  expect_s3_class(as.data.frame(resp_list), "data.frame")
})
