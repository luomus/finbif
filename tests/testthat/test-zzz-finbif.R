if (has_dev_api()) {

  token <- Sys.getenv("FINBIF_ACCESS_TOKEN")
  Sys.setenv(FINBIF_ACCESS_TOKEN = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"))

  options(finbif_api_url = "https://apitest.laji.fi")

  assignInNamespace("var_names", finbif:::var_names_test, "finbif")
  assignInNamespace("filter_names", finbif:::filter_names_test, "finbif")
  assignInNamespace("has_value", finbif:::has_value_test, "finbif")
  assignInNamespace(
    "lite_download_file_vars", finbif:::lite_download_file_vars_test, "finbif"
  )
  assignInNamespace("cite_file_vars", finbif:::cite_file_vars_test, "finbif")

  invisible(vcr_configure(dir = "../api-dev-cassettes"))

  files <- list.files(pattern = "^test-finbif.*\\.[rR]$")

  for (file in files) source(file, local = TRUE)

  options(finbif_api_url = "https://api.laji.fi")

  Sys.setenv(FINBIF_ACCESS_TOKEN = token)

}
