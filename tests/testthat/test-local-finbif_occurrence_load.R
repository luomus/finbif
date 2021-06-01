suppressMessages(insert_cassette("finbif_occurrence_load"))

test_that(
  "can load a file", {

    skip_on_cran()

    file <- 49381L
    nrows <- 280L

    zip <- sprintf("HBF.%s.zip", file)
    tsv <- sprintf("rows_HBF.%s.tsv", file)
    unzip(zip, tsv, exdir = tempdir())
    tsv <- paste(tempdir(), tsv, sep = "/")

    expect_identical(
      335L,
      finbif_occurrence_load(tsv, count_only = TRUE)
    )

    finbif_clear_cache()

    expect_identical(
      nrows,
      finbif_occurrence_load(tsv, n = nrows, count_only = TRUE)
    )

    options(finbif_cache_path = "../write-files")

    file_path <-
      "../write-files/finbif_cache_file_4c64a068da60f708c4e928701ec538ef"

    expect_snapshot_value(
      finbif_occurrence_load(file, quiet = TRUE, tzone = "Etc/UTC")[
        seq(nrows),
      ],
      style = "json2"
    )

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
        file_full, tzone = "Etc/UTC", write_file = file_path
        )[seq(nrows), ],
      style = "json2"
    )

    expect_snapshot_value(
      finbif_occurrence_load(
        file_full, tzone = "Etc/UTC", write_file = file_path
        )[seq(nrows), ],
      style = "json2"
    )

    expect_snapshot_value(
      finbif_occurrence_load(zip, tzone = "Etc/UTC")[seq(nrows), ],
      style = "json2"
    )

    expect_snapshot_value(
      finbif_occurrence_load(zip, n = nrows, tzone = "Etc/UTC"),
      style = "json2"
    )

    expect_snapshot_value(
      finbif_occurrence_load(zip, select = "all", n = nrows, tzone = "Etc/UTC"),
      style = "json2"
    )

  }

)

test_that(
  "with invalid URL returns an error message", {

    skip_on_cran()

    expect_error(
      suppressMessages(finbif_occurrence_load("http://tun.fi/HBF.0")),
      "File request failed"
    )

    options(finbif_cache_path = tempdir())

  }
)

suppressMessages(eject_cassette("finbif_occurrence_load"))
