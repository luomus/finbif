context("Rerunning all test against dev")

if (has_dev_api()) {

  token <- Sys.getenv("FINBIF_ACCESS_TOKEN")
  Sys.setenv(FINBIF_ACCESS_TOKEN = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"))

  options(finbif_api_url = "apitest.laji.fi")

  assignInNamespace("var_names", finbif:::var_names_test, "finbif")

  invisible(vcr::vcr_configure(dir = "../api-dev-cassettes"))

  files <- list.files(pattern = "^test-finbif.*\\.[rR]$")

  for (file in files) source(file, local = TRUE)

  options(finbif_api_url = "api.laji.fi")

  Sys.setenv(FINBIF_ACCESS_TOKEN = token)

}
