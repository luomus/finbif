has_dev_token <- !identical(nchar(Sys.getenv("FINBIF_DEV_ACCESS_TOKEN")), 0L)

not_cran <- identical(Sys.getenv("NOT_CRAN"), "true")

is_main_branch <- FALSE

has_gert <- requireNamespace("gert", quietly = TRUE)

has_vcr <- requireNamespace("vcr", quietly = TRUE)

has_here <- requireNamespace("here", quietly = TRUE)

has_grd <- requireNamespace("grDevices", quietly = TRUE)

if (has_vcr && not_cran) {

  library("vcr", quietly = TRUE)

  if (has_grd) {

    library("grDevices", quietly = TRUE)

    save_svg <- function(code, width = 7, height = 7) {
      path <- tempfile(fileext = ".svg")
      grDevices::svg(path, width = width, height = height, antialias = "none")
      on.exit(dev.off())
      code

      path
    }

  }

  if (has_here) {

    library("here", quietly = TRUE)

    cassettes <- here::here("tests", "cassettes")

    has_git <- dir.exists(here::here(".git"))

    is_main_branch <- identical(Sys.getenv("BRANCH"), "main")

    if (has_gert && has_git) {

      suppressPackageStartupMessages(library("gert", quietly = TRUE))

      is_main_branch <- identical(gert::git_branch(), "main")

    }

    if (!is_main_branch && has_dev_token) {

      Sys.setenv(FINBIF_ACCESS_TOKEN = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"))

      options(finbif_api_url = "https://apitest.laji.fi")

      assignInNamespace(
        "var_names",
        finbif:::var_names_test,
        "finbif"
      )
      assignInNamespace(
        "filter_names",
        finbif:::filter_names_test,
        "finbif"
      )
      assignInNamespace(
        "has_value",
        finbif:::has_value_test,
        "finbif"
      )
      assignInNamespace(
        "lite_download_file_vars",
        finbif:::lite_download_file_vars_test,
        "finbif"
      )
      assignInNamespace(
        "cite_file_vars",
        finbif:::cite_file_vars_test,
        "finbif"
      )

      cassettes <- here::here("tests", "api-dev-cassettes")

    }

    if (dir.exists(cassettes)) {

      invisible(
        vcr::vcr_configure(
          dir = cassettes,
          write_disk_path = "../write-files",
          record = "new_episodes",
          filter_sensitive_data = list(
            "<finbif_token>"     = Sys.getenv(
              "FINBIF_ACCESS_TOKEN"
            ),
            "<finbif_dev_token>" = Sys.getenv(
              "FINBIF_DEV_ACCESS_TOKEN"
            ),
            "<finbif_dl_token>"  = Sys.getenv(
              "FINBIF_RESTRICTED_FILE_ACCESS_TOKEN"
            )
          )
        )
      )

    }

  }

}  else {

  dummy <- function(...) NULL

  vcr_configure <- dummy
  insert_cassette <- dummy
  eject_cassette <- dummy

  Sys.setenv(NOT_CRAN = "false")

}

options(stringsAsFactors = FALSE)
