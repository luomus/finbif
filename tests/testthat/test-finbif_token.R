context("Checking FinBIF access token")

test_that("is a string", {

  tokn <- Sys.getenv("FINBIF_ACCESS_TOKEN")
  if (identical(tokn, "")) {
    expect_null(token())
  } else {
    expect_type(token(), "character")
  }

})

test_that("retrival produces a message", {

  expect_message(token(quiet = FALSE))

})

test_that("returns NULL when unset", {

  tokn <- Sys.getenv("FINBIF_ACCESS_TOKEN")
  Sys.unsetenv("FINBIF_ACCESS_TOKEN")
  expect_null(token())
  Sys.setenv(FINBIF_ACCESS_TOKEN = tokn)

})
