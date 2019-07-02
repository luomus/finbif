context("FinBIF api get request")

test_that(
  "missing token returns error", {
    token <- Sys.getenv("FINBIF_ACCESS_TOKEN")
    Sys.unsetenv("FINBIF_ACCESS_TOKEN")
    expect_error(finbif_api_get(), "Access token for FinBIF has not been set")
    Sys.setenv(FINBIF_ACCESS_TOKEN = token)
  }
)

vcr::use_cassette(
  "finbif_api_get", {
    test_that(
      "returns valid data", {
        resp_list1 <- finbif_api_get(
          path = "v0/warehouse/query/list",
          query = list(page = 1, pageSize = 1, selected = "unit.unitId")
        )
        expect_s3_class(resp_list1, "finbif_api")
      }
    )

    test_that(
      "with wrong field returns an error message", {
        expect_error(
          finbif_api_get(
            path = "v0/warehouse/query/list",
            query = list(page = 1, pageSize = 1, selected = "not_a_field")
          ),
          "API request failed"
        )
      }
    )

    test_that(
      "not receiving JSON returns error message", {
        expect_error(
          finbif_api_get(
            path = "v0/warehouse/query/list",
            query = list(
              format = "xml", page = 1, pageSize = 1, selected = "unit.unitId"
            )
          ),
          "API did not return json"
        )
      }
    )
  },
  preserve_exact_body_bytes = TRUE
)
