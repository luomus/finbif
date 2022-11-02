if (requireNamespace("webfakes", quietly = TRUE)) {

  library("webfakes", quietly = TRUE)

  app <- new_app()

  app[["get"]](
    "/HBF.49381",
    function(req, res) {
      file <- "HBF.49381.zip"
      ans <- readBin(file, "raw", n = file.info(file)[["size"]])
      res[["send"]](ans)
    }
  )

  app[["get"]](
    "/HBF.0",
    function(req, res) {
      res[["set_status"]](404L)
      res[["send"]]("")
    }
  )

  api <- local_app_process(app, .local_envir = teardown_env(), port = 36761)

  options(finbif_dl_url = api[["url"]]())

} else {

  Sys.setenv(NOT_CRAN = "false")

}

test_that(
  "can load a file", {

    skip_on_cran()

    file <- 49381L
    nrows <- 280L

    zip <- sprintf("HBF.%s.zip", file)
    tsv <- sprintf("rows_HBF.%s.tsv", file)
    unzip(zip, tsv, exdir = tempdir())
    file.copy(paste(tempdir(), tsv, sep = "/"), tsv)

    options(finbif_tz = "Etc/UTC")

    expect_identical(
      335L,
      finbif_occurrence_load(tsv, count_only = TRUE)
    )

    finbif_clear_cache()

    expect_identical(
      335L,
      finbif_occurrence_load(tsv, n = nrows, count_only = TRUE)
    )

    options(finbif_cache_path = "../write-files", finbif_tz = Sys.timezone())

    file_path <-
      "../write-files/finbif_cache_file_3db6439c7601e4401b8a27a2094919a3"

    expect_snapshot_value(
      finbif_occurrence_load(file, quiet = TRUE, tzone = "Etc/UTC")[
        seq(nrows),
      ],
      style = "json2"
    )

    expect_snapshot_value(
      finbif_occurrence_load(tsv, quiet = TRUE, tzone = "Etc/UTC")[
        seq(nrows),
      ],
      style = "json2"
    )

    file.remove(tsv)

    options(finbif_cache_path = NULL)

    capture.output(
      with_progress <- suppressMessages(
        finbif_occurrence_load(file, tzone = "Etc/UTC", write_file = file_path)[
          seq(nrows),
        ]
      )
    )

    expect_snapshot_value(with_progress, style = "json2")

    file_full <- paste0("http://tun.fi/HBF.", file)

    expect_snapshot_value(
     finbif_occurrence_load(
       file_full, n = 0, tzone = "Etc/UTC", write_file = file_path, dt = FALSE,
       keep_tsv = TRUE
     ),
     style = "json2", ignore_attr = "url"
    )

    expect_snapshot_value(
      finbif_occurrence_load(
        file_full, tzone = "Etc/UTC", write_file = file_path, dt = FALSE,
        keep_tsv = TRUE
      )[seq(nrows), ],
      style = "json2", ignore_attr = "url"
    )

    expect_snapshot_value(
      finbif_occurrence_load(zip, tzone = "Etc/UTC")[seq(nrows), ],
      style = "json2"
    )

    expect_snapshot_value(
      finbif_occurrence_load(
        zip, n = nrows, tzone = "Etc/UTC",
        facts = list(record = c("imageCount", "imageUrl", "areaInSqMeters"))
      ),
      style = "json2"
    )

    expect_warning(
      finbif_occurrence_load(
        zip, n = nrows, tzone = "Etc/UTC",
        facts = list(record = c("not a fact"))
      ),
      "Selected fact"
    )

    expect_snapshot_value(
      capture.output(
        print(
          finbif_occurrence_load(
            zip, select = "short", n = nrows, tzone = "Etc/UTC"
          )[c("recID", "recOrder", "lonWGS84", "latWGS84")]
        )
      ),
      style = "json2"
    )

    expect_snapshot_value(
      capture.output(
        print(
          finbif_occurrence_load(
            zip, select = "all", n = nrows, tzone = "Etc/UTC"
          )
        )
      ),
      style = "json2"
    )

    expect_warning(
      capture.output(
        print(
          finbif_occurrence_load("HBF.6968.zip", facts = list(event = "fact"))
        )
      )
    )

    expect_snapshot_value(
      capture.output(
        print(
          finbif_occurrence_load("HBF.6968.zip", select = "all")
        )
      ),
      style = "json2"
    )

  }

)

if (!identical(.Platform$OS.type, "windows")) {

  test_that(
    "can load data from a lite download", {

      expect_snapshot_value(
        finbif_occurrence_load("laji-data.tsv", tzone = "Etc/UTC", dt = FALSE),
        style = "json2"
      )

      expect_snapshot_value(
        finbif_occurrence_load("laji-data-pap.tsv", tzone = "Etc/UTC"),
        style = "json2"
      )

      skip_on_cran()

      expect_snapshot_value(
        finbif_occurrence_load("laji-data.ods", tzone = "Etc/UTC"),
        style = "json2", ignore_attr = "url"
      )

      expect_snapshot_value(
        finbif_occurrence_load("laji-data.xlsx", tzone = "Etc/UTC"),
        style = "json2", ignore_attr = "url"
      )

    }
  )

}

test_that(
  "with invalid URL returns an error message", {

    skip_on_cran()

    expect_error(
      suppressMessages(finbif_occurrence_load("http://tun.fi/HBF.0")),
      "File request failed"
    )

  }
)

test_that(
  "large download returns an error message", {

    skip_on_cran()

    Sys.setenv("FINBIF_FILE_SIZE_LIMIT" = "52e3")

    file_path <-
      "../write-files/finbif_cache_file_3db6439c7601e4401b8a27a2094919a3"

    expect_error(
      finbif_occurrence_load(
        49381L, cache = FALSE, quiet = TRUE, write_file = file_path
      ),
      "File download too large"
    )

    Sys.unsetenv("FINBIF_FILE_SIZE_LIMIT")

  }
)
