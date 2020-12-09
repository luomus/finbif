context("Checking FinBIF API get request")

test_that(
  "with missing token returns error", {

    token <- Sys.getenv("FINBIF_ACCESS_TOKEN")
    Sys.unsetenv("FINBIF_ACCESS_TOKEN")

    expect_error(api_get(), "Access token for FinBIF has not been set")

    Sys.setenv(FINBIF_ACCESS_TOKEN = token)

  }
)

use_cassette(
  "api_get", {

    test_that(
      "with wrong var returns an error message", {

        skip_on_cran()

        expect_error(
          api_get(
            path = "warehouse/query/unit/list",
            query = list(page = 1, pageSize = 1, selected = "not_a_var"),
            cache = TRUE
          ),
          "API request failed"
        )

      }
    )

    test_that(
      "that doesn't receive JSON returns an error message", {

        skip_on_cran()

        expect_error(
          api_get(
            path = "warehouse/query/unit/list",
            query = list(
              format = "xml", page = 1, pageSize = 1, selected = "unit.unitId"
            ),
            cache = TRUE
          ),
          "API did not return json"
        )

      }
    )

  },
  preserve_exact_body_bytes = TRUE
)
