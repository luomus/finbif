library("vcr")
library("vdiffr")
invisible(vcr::vcr_configure(
  dir = "../cassettes",
  record = "new_episodes",
  clean_outdated_http_interactions = TRUE,
  filter_sensitive_data = list(
    "<finbif_token>" = Sys.getenv("FINBIF_ACCESS_TOKEN")
  )
))

has_dev_api <- function() nchar(Sys.getenv("FINBIF_DEV_ACCESS_TOKEN")) > 0L

is_dev_api <-
  function() identical(getOption("finbif_api_url"), "apitest.laji.fi")
