test_that("requesting token works", {

  skip_on_cran()

  op <- options()

  f <- tempfile()

  if (
    requireNamespace("callr", quietly = TRUE) &&
      requireNamespace("webfakes", quietly = TRUE)
  ) {

    bg <- callr::r_bg(

      function(file) {

        app <- webfakes::new_app()

        app[["post"]](
          "/api-users",
          function(req, res) {
            res[["send_json"]]("")
          }
        )

        app[["post"]](
          "/api-users/renew",
          function(req, res) {
            res[["send_json"]]("")
          }
        )

        web <- webfakes::local_app_process(app)

        cat(c(web[["url"]](), "."), file = file, sep = "\n")

        Sys.sleep(60L)

      },
      list(file = f)
    )

    while (!file.exists(f) || length(url <- readLines(f, warn = FALSE)) < 2L) {}

    options(finbif_api_url = sub("/$", "", url[[1L]]), finbif_rate_limit = Inf)

    tokn <- Sys.getenv("FINBIF_ACCESS_TOKEN")

    Sys.unsetenv("FINBIF_ACCESS_TOKEN")

    expect_error(
      finbif_occurrence(), "Access token for FinBIF has not been set"
    )

    expect_message(
      finbif_request_token("test@email"),
      "A personal access token for api.laji.fi"
    )

    expect_message(
      finbif_renew_token("test@email"),
      "A personal access token for api.laji.fi"
    )

  }

  Sys.setenv(FINBIF_ACCESS_TOKEN = tokn)

  expect_message(
    finbif_request_token("em"), "An access token has already been set"
  )

  expect_error(
    finbif_occurrence(restricted_api = "XXXXXXXXXX"),
    "Restricted API token declared but token is unset"
  )

  options(op)

})
