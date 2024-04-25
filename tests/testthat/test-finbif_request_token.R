if (requireNamespace("webfakes", quietly = TRUE)) {

  library("webfakes")

  app <- new_app()

  app$post(
    sprintf("/%s/api-users", getOption("finbif_api_version")),
    function(req, res) {
      res$send_json("")
    }
  )

  app$post(
    "/test-error/api-users",
    function(req, res) {
      res$set_status(404L)
      res$send_json("")
    }
  )

  http <- local_app_process(app)

  http[["start"]]()

} else {

  Sys.setenv(NOT_CRAN = "false")

}

test_that(
  "can request token", {

    skip_on_cran()

    tokn <- Sys.getenv("FINBIF_ACCESS_TOKEN")

    Sys.unsetenv("FINBIF_ACCESS_TOKEN")

    op <- options()

    options(finbif_api_url = http$url())

    expect_s3_class(suppressMessages(finbif_request_token("em")), "finbif_api")

    options(finbif_api_version = "test-error")

    expect_error(finbif_request_token("em", quiet = TRUE))

    options(op)

    Sys.setenv(FINBIF_ACCESS_TOKEN = tokn)

  }
)

test_that(
  "reports that token has been set", {

    skip_on_cran()

    expect_message(
      finbif_request_token("em"), "An access token has already been set"
    )

  }
)
