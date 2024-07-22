library("vcr")

branch <- Sys.getenv("BRANCH")

gh_ref <- Sys.getenv("GITHUB_REF_NAME")

is_dev_branch <- identical(branch, "dev") || identical(gh_ref, "dev")

has_dev_token <- !identical(nchar(Sys.getenv("FINBIF_DEV_ACCESS_TOKEN")), 0L)

if (is_dev_branch && has_dev_token) {

  Sys.setenv(FINBIF_ACCESS_TOKEN = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"))

  options(finbif_api_url = "https://apitest.laji.fi")

}

invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  filter_sensitive_data = list(
    "<finbif_token>" = Sys.getenv("FINBIF_ACCESS_TOKEN"),
    "<finbif_dev_token>" = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"),
    "<finbif_dl_token>" = Sys.getenv("FINBIF_RESTRICTED_FILE_ACCESS_TOKEN")
  )
))

vcr::check_cassette_names()
