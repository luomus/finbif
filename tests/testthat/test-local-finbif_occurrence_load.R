test_that(
  "can load a file", {

    zip  <- "HBF.49381.zip"
    tsv  <- "rows_HBF.49381.tsv"
    unzip(zip, tsv, exdir = tempdir())
    tsv <- paste(tempdir(), tsv, sep = "/")
    nrows <- 280L

    expect_snapshot_value(
      finbif_occurrence_load(zip, tzone = "Etc/UTC")[seq(nrows), ],
      style = "json2"
    )

    expect_identical(
      335L,
      finbif_occurrence_load(tsv, count_only = TRUE)
    )

    expect_snapshot_value(
      finbif_occurrence_load(zip, n = nrows, tzone = "Etc/UTC"),
      style = "json2"
    )

    expect_identical(
      nrows,
      finbif_occurrence_load(tsv, n = nrows, count_only = TRUE)
    )

  }

)
