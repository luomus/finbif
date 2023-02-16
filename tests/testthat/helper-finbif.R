options(finbif_rate_limit = Inf)

has_dev_token <- !identical(nchar(Sys.getenv("FINBIF_DEV_ACCESS_TOKEN")), 0L)

not_cran <- identical(Sys.getenv("NOT_CRAN"), "true")

is_dev_branch <- FALSE

has_vcr <- requireNamespace("vcr", quietly = TRUE)

has_grd <- requireNamespace("grDevices", quietly = TRUE)

dummy <- function(...) NULL

if (has_vcr && not_cran) {

  library("vcr", quietly = TRUE)

  cassettes <- "../cassettes"

  is_dev_branch <- identical(Sys.getenv("BRANCH"), "dev")

  if (is_dev_branch && has_dev_token) {

    Sys.setenv(FINBIF_ACCESS_TOKEN = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"))

    options(finbif_api_url = "https://apitest.laji.fi")

  }

  if (dir.exists(cassettes)) {

    vcr::vcr_configure(
      dir = cassettes,
      write_disk_path = "../write-files",
      record = "new_episodes",
      filter_sensitive_data = list(
        "<finbif_token>" = Sys.getenv("FINBIF_ACCESS_TOKEN"),
        "<finbif_dev_token>" = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"),
        "<finbif_dl_token>" = Sys.getenv("FINBIF_RESTRICTED_FILE_ACCESS_TOKEN")
      )
    )

  }

} else {

  Sys.setenv(NOT_CRAN = "false")

  insert_cassette <- dummy

  eject_cassette <- dummy

}
