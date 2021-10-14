dummy <- function(...) NULL

if (requireNamespace("vcr", quietly = TRUE) && dir.exists("../cassettes")) {

  library("vcr")
  invisible(
    vcr::vcr_configure(
      dir = "../cassettes",
      write_disk_path = "../write-files",
      record = "new_episodes",
      filter_sensitive_data = list(
        "<finbif_token>"     = Sys.getenv("FINBIF_ACCESS_TOKEN"),
        "<finbif_dev_token>" = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"),
        "<finbif_dl_token>" = Sys.getenv("FINBIF_RESTRICTED_FILE_ACCESS_TOKEN")
      )
    )
  )

} else {

  vcr_configure <- dummy
  insert_cassette <- dummy
  eject_cassette <- dummy
  Sys.setenv(NOT_CRAN = "false")

}

has_dev_api <- function() nchar(Sys.getenv("FINBIF_DEV_ACCESS_TOKEN")) > 0L

is_dev_api <-
  function() identical(getOption("finbif_api_url"), "https://apitest.laji.fi")

if (requireNamespace("grDevices")) {

  save_svg <- function(code, width = 7, height = 7) {
    path <- tempfile(fileext = ".svg")
    svg(path, width = width, height = height, antialias = "none")
    on.exit(dev.off())
    code

    path
  }

}

options(stringsAsFactors = FALSE)
