context('FinBIF api get request')

test_that("returns valid data", {
  vcr::use_cassette("finbif_api_get", {
    resp_list <- finbif_api_get(
      path = "v0/warehouse/query/list",
      query = list(page = 1, pageSize = 1, selected = "unit.unitId")
    )
  }, preserve_exact_body_bytes = TRUE)

  expect_s3_class(resp_list, "finbif_api")

})
