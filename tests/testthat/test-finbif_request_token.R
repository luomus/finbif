test_that("requesting token works", {

  skip_on_cran()

  op <- options()

  f <- tempfile()

  if (
    requireNamespace("callr", quietly = TRUE) &&
      requireNamespace("webfakes", quietly = TRUE)
  ) {

    bg <- callr::r_bg(
      function(file, version) {

        app <- webfakes::new_app()

        app[["post"]](
          sprintf("/%s/api-users", version),
          function(req, res) {
            res[["send_json"]]("")
          }
        )

        app[["post"]](
          "/test-error/api-users",
          function(req, res) {
            res[["set_status"]](404L)
            res[["send_json"]]("")
          }
        )

        web <- webfakes::local_app_process(app)

        cat(c(web[["url"]](), "."), file = file, sep = "\n")

        Sys.sleep(60L)

      },
      list(file = f, version = getOption("finbif_api_version"))
    )

    while (!file.exists(f) || length(url <- readLines(f, warn = FALSE)) < 2L) {}

    options(finbif_api_url = sub("/$", "", url[[1L]]), finbif_rate_limit = Inf)

    tokn <- Sys.getenv("FINBIF_ACCESS_TOKEN")

    Sys.unsetenv("FINBIF_ACCESS_TOKEN")

    expect_error(
      finbif_occurrence(), "Access token for FinBIF has not been set"
    )

    expect_message(
      finbif_request_token("em"), "A personal access token for api.laji.fi"
    )

    options(finbif_api_version = "test-error")

    expect_error(
      finbif_request_token("em"), "API request failed"
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
