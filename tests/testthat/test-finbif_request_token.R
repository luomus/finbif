context("Checking FinBIF API token request")

test_that(
  "reports that token has been set", {

    skip_on_cran()

    expect_message(
      finbif_request_token(), "An access token has already been set"
    )

  }
)
