test_that(
  "can load a file", {

    zip  <- "HBF.49381.zip"
    tsv  <- "rows_HBF.49381.tsv"
    unzip(zip, tsv, exdir = tempdir())
    tsv <- paste(tempdir(), tsv, sep = "/")

    expect_snapshot_value(
      finbif_occurrence_load(zip, tzone = "Etc/UTC"), style = "json2"
    )

    expect_identical(
      335L,
      finbif_occurrence_load(tsv, count_only = TRUE)
    )

    expect_snapshot_value(
      finbif_occurrence_load(zip, n = 300L, tzone = "Etc/UTC"), style = "json2"
    )

    expect_identical(
      300L,
      finbif_occurrence_load(tsv, n = 300L, count_only = TRUE)
    )

  }

)
