context("FinBIF access token")

test_that("Access token is a string", {
  token <- Sys.getenv("FINBIF_ACCESS_TOKEN")
  if (identical(token, "")) {
    expect_null(finbif_token())
  } else {
    expect_type(finbif_token(), "character")
  }
})

test_that("Access token retrival produces a message", {
  expect_message(finbif_token(quiet = FALSE))
})

test_that("Access token returns NULL when unset", {
  token <- Sys.getenv("FINBIF_ACCESS_TOKEN")
  Sys.unsetenv("FINBIF_ACCESS_TOKEN")
  expect_null(finbif_token())
  Sys.setenv(FINBIF_ACCESS_TOKEN = token)
})
