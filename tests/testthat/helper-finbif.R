dummy <- function(...) NULL

if (requireNamespace("vcr")) {

  library("vcr")
  invisible(
    vcr::vcr_configure(
      dir = "../cassettes",
      record = "new_episodes",
      filter_sensitive_data = list(
        "<finbif_token>"     = Sys.getenv("FINBIF_ACCESS_TOKEN"),
        "<finbif_dev_token>" = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN")
      )
    )
  )

} else {

  vcr_configure <- dummy
  use_cassette <- dummy

}

has_dev_api <- function() nchar(Sys.getenv("FINBIF_DEV_ACCESS_TOKEN")) > 0L

is_dev_api <-
  function() identical(getOption("finbif_api_url"), "apitest.laji.fi")

save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code

  path
}
