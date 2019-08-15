context("Checking FinBIF API token request")

test_that(
  "reports that token has been set", {

    expect_message(
      finbif_request_token(), "An access token has already been set"
    )

  }
)
