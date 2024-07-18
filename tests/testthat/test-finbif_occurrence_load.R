test_that("download imports work", {

  skip_on_cran()

  op <- options()

  options(finbif_rate_limit = Inf)

  vcr::use_cassette("finbif_occurrence_load", {

    HBF_49381_zip_mem <- finbif_occurrence_load(
      "HBF.49381.zip",
      select = "short",
      tzone = "Etc/UTC",
      quiet = TRUE
    )

    options(finbif_cache_path = tempdir())

    HBF_49381_zip_file <- finbif_occurrence_load(
      "HBF.49381.zip",
      select = "short",
      tzone = "Etc/UTC",
      quiet = TRUE
    )

    HBF_49382_zip_file <- finbif_occurrence_load(
      "HBF.49382.zip",
      tzone = "Etc/UTC",
      quiet = TRUE
    )

    HBF_6968_zip_file <- finbif_occurrence_load(
      "HBF.6968.zip",
      tzone = "Etc/UTC",
      quiet = TRUE
    )

    HBF_6960_zip_file <- finbif_occurrence_load(
      "HBF.6960.zip",
      tzone = "Etc/UTC",
      quiet = TRUE,
      dt = FALSE
    )

    laji_data_tsv <- finbif_occurrence_load(
      "laji-data.tsv", select = "all", tzone = "Etc/UTC", quiet = TRUE
    )

    laji_data_pap_tsv <- finbif_occurrence_load(
      "laji-data-pap.tsv", tzone = "Etc/UTC", quiet = TRUE
    )

    laji_data_new_col_tsv <- finbif_occurrence_load(
      "laji-data-new-col.tsv", tzone = "Etc/UTC", quiet = TRUE
    )

    laji_data_ods <- finbif_occurrence_load(
      "laji-data.ods", dt = FALSE, n = 0, tzone = "Etc/UTC", quiet = TRUE
    )

    laji_data_xlsx <- finbif_occurrence_load(
      "laji-data.xlsx", tzone = "Etc/UTC", quiet = TRUE
    )

  })

  expect_snapshot_value(
    HBF_49381_zip_mem, style = "json2", ignore_attr = "url"
  )

  expect_snapshot_value(
    HBF_49381_zip_file, style = "json2", ignore_attr = "url"
  )

  expect_snapshot_value(
    HBF_49382_zip_file, style = "json2", ignore_attr = "url"
  )

  expect_snapshot_value(
    HBF_6968_zip_file, style = "json2", ignore_attr = "url"
  )

  expect_snapshot_value(
    HBF_6960_zip_file, style = "json2", ignore_attr = "url"
  )

  expect_equal(
    finbif_occurrence_load("HBF.49381.zip", count_only = TRUE, quiet = TRUE),
    335L
  )

  expect_snapshot_value(
    laji_data_tsv, style = "json2", ignore_attr = "url"
  )

  expect_snapshot_value(
    laji_data_pap_tsv, style = "json2", ignore_attr = "url"
  )

  expect_snapshot_value(
    laji_data_new_col_tsv, style = "json2", ignore_attr = "url"
  )

  expect_snapshot_value(
    laji_data_ods, style = "json2", ignore_attr = "url"
  )

  expect_snapshot_value(
    laji_data_xlsx, style = "json2", ignore_attr = "url"
  )

  tmp <- tempfile()

  bg <- callr::r_bg(
    function(file) {

      app <- webfakes::new_app()

      app[["get"]](
        "/HBF.49381",
        function(req, res) {
          file <- "HBF.49381.zip"
          ans <- readBin(file, "raw", n = file.info(file)[["size"]])
          res[["send"]](ans)
        }
      )

      app[["get"]](
        "/HBF.6968",
        function(req, res) {
          file <- "HBF.6968.zip"
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

      web <- webfakes::local_app_process(app)

      cat(c(web[["url"]](), "."), file = file, sep = "\n")

      Sys.sleep(60L)

    },
    list(file = tmp)
  )

  while (!file.exists(tmp) || length(url <- readLines(tmp)) < 2L) {}

  options(finbif_dl_url = sub("/$", "", url[[1L]]))

  expect_error(
    finbif_occurrence_load("http://tun.fi/HBF.0", quiet = TRUE),
    "File request failed"
  )

  Sys.setenv("FINBIF_FILE_SIZE_LIMIT" = "52e3")

  expect_error(
    finbif_occurrence_load(49381, quiet = TRUE), "File download too large"
  )

  Sys.unsetenv("FINBIF_FILE_SIZE_LIMIT")

  expect_warning(
    tun_fi_HBF_49381 <- finbif_occurrence_load(
      "https://tun.fi/HBF.49381",
      facts = list(
        record = c("imageCount", "imageUrl", "areaInSqMeters", "not_a_fact")
      ),
      tzone = "Etc/UTC",
      keep_tsv = TRUE,
      n = 300,
      dt = FALSE,
      quiet = TRUE
    ),
    "could not be found in dataset"
  )

  expect_snapshot_value(tun_fi_HBF_49381, style = "json2", ignore_attr = "url")

  options(finbif_cache_path = NULL)

  expect_snapshot_value(
    capture.output(
      finbif_occurrence_load(
        49381,
        tzone = "Etc/UTC"
      )
    ),
    style = "json2",
    ignore_attr = "url"
  )

  expect_snapshot_value(
    finbif_occurrence_load(
      49381,
      tzone = "Etc/UTC",
      quiet = TRUE
    ),
    style = "json2",
    ignore_attr = "url"
  )

  expect_warning(
    finbif_occurrence_load(
      6968,
      facts = list(event = "not_a_fact"),
      tzone = "Etc/UTC",
      quiet = TRUE
    ),
    "could not be found in dataset"
  )

  bg[["kill"]]()

  con <- file()

  options(finbif_cache_path = con)

  expect_error(
    finbif_occurrence_load(49381),
    "Database cache cannot be used for FinBIF downloads."
  )

  close(con)

  options(op)

})
