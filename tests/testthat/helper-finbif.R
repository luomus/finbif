options(finbif_rate_limit = Inf)

Sys.setenv(DEPRECATION_WARNING = FALSE)

has_vcr <- requireNamespace("vcr", quietly = TRUE)

not_cran <- identical(Sys.getenv("NOT_CRAN"), "true")

if (has_vcr && not_cran) {

  library("vcr", quietly = TRUE)

  branch <- Sys.getenv("BRANCH")

  gh_ref <- Sys.getenv("GITHUB_REF_NAME")

  is_dev_branch <- identical(branch, "dev") || identical(gh_ref, "dev")

  has_dev_token <- !identical(nchar(Sys.getenv("FINBIF_DEV_ACCESS_TOKEN")), 0L)

  if (is_dev_branch && has_dev_token) {

    Sys.setenv(FINBIF_ACCESS_TOKEN = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"))

    options(finbif_api_url = "https://apitest.laji.fi")

  }

  vcr::vcr_configure(
    dir = "../cassettes",
    write_disk_path = "../write-files",
    record = "new_episodes",
    filter_sensitive_data = list(
      "<finbif_token>" = Sys.getenv("FINBIF_ACCESS_TOKEN"),
      "<finbif_dev_token>" = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"),
      "<finbif_dl_token>" = Sys.getenv("FINBIF_RESTRICTED_FILE_ACCESS_TOKEN")
    )
  )

} else {

  Sys.setenv(NOT_CRAN = "false")

  dummy <- function(...) NULL

  insert_cassette <- dummy

  eject_cassette <- dummy

}
