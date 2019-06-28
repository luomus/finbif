context('FinBIF api token request')

test_that("requesting a token when one is set is reported", {
  expect_message(finbif_request_token(), "An access token has already been set")
})

## Might be used when https://github.com/ropensci/vcr/issues/70 is closed
# vcr::use_cassette(
#   "finbif_request_token", {
#     test_that("requesting a token works", {
#       token <- Sys.getenv("FINBIF_ACCESS_TOKEN")
#       Sys.unsetenv("FINBIF_ACCESS_TOKEN")
#       expect_message(
#         resp <- finbif_request_token(
#           sprintf("rpkg.api.test%s@notareal.email", sample(1e5, 1))
#         ),
#         "A personal access token for"
#       )
#       expect_s3_class(resp, "finbif_api")
#       Sys.setenv(FINBIF_ACCESS_TOKEN = token)
#     })
#
#     test_that("requesting a token with invalid email returns error", {
#       token <- Sys.getenv("FINBIF_ACCESS_TOKEN")
#       Sys.unsetenv("FINBIF_ACCESS_TOKEN")
#       expect_error(
#         finbif_request_token("rpkg.api.test.invalid.email"),
#         "API request failed"
#       )
#       Sys.setenv(FINBIF_ACCESS_TOKEN = token)
#     })
#   },
#   preserve_exact_body_bytes = TRUE,
#   match_requests_on = c('method', 'uri', 'body')
# )
