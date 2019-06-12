library("vcr")
invisible(vcr::vcr_configure(
  dir = "../cassettes",
  filter_sensitive_data = list(
    "<finbif_token>" = Sys.getenv("FINBIF_ACCESS_TOKEN")
  )
))
