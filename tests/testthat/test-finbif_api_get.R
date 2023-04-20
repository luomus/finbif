test_that(
  "with missing token returns error", {

    token <- Sys.getenv("FINBIF_ACCESS_TOKEN")
    Sys.unsetenv("FINBIF_ACCESS_TOKEN")

    expect_error(api_get(), "Access token for FinBIF has not been set")

    Sys.setenv(FINBIF_ACCESS_TOKEN = token)

    expect_error(
      fb_occurrence(restricted_api = "KEY"),
      "Restricted API token declared but token is unset"
    )

  }
)

suppressMessages(insert_cassette("api_get"))

test_that(
  "with wrong var returns an error message", {

    skip_on_cran()

    expect_error(
      suppressMessages(
        api_get(
          list(
            path = "warehouse/query/unit/list",
            query = list(page = 1, pageSize = 1, selected = "not_a_var"),
            cache = TRUE
          )
        )
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
        list(
          path = "warehouse/query/unit/list",
          query = list(
            format = "xml", page = 1, pageSize = 1, selected = "unit.unitId"
          ),
          cache = TRUE
        )
      ),
      "API did not return json"
    )

  }
)

suppressMessages(eject_cassette("api_get"))

suppressMessages(insert_cassette("api_get_with_email"))

test_that(
  "query works with email", {

    skip_on_cran()

    options(finbif_email = "noreply@laji.fi")

    expect_s3_class(
      api_get(
        list(
          path = "warehouse/query/unit/list",
          query = list(page = 1, pageSize = 1, selected = "unit.unitId"),
          cache = TRUE
        )
      ),
      "finbif_api"
    )

    options(finbif_email = NULL)

  }
)

suppressMessages(eject_cassette("api_get_with_email"))
