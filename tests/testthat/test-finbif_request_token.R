context("FinBIF api token request")

test_that(
  "requesting a token when one is set is reported", {
    expect_message(
      finbif_request_token(), "An access token has already been set"
    )
  }
)
