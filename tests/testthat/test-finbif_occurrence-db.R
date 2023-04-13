suppressMessages(insert_cassette("finbif_occurrence_db"))

has_dbi <- requireNamespace("DBI", quietly = TRUE)

has_rsqlite <- requireNamespace("RSQLite", quietly = TRUE)

if (has_dbi && has_rsqlite) {

  library("DBI", quietly = TRUE)

  library("RSQLite", quietly = TRUE)

  db <- dbConnect(SQLite(), tempfile())

} else {

  Sys.setenv(NOT_CRAN = "false")

}

test_that(
  "database caching works", {

    skip_on_cran()

    op <- getOption("finbif_cache_path")

    options(finbif_cache_path = db)

    expect_s3_class(finbif_occurrence(), "finbif_occ")

    expect_s3_class(finbif_occurrence(), "finbif_occ")

    finbif_clear_cache()

    expect_s3_class(finbif_occurrence(cache = 1e-9), "finbif_occ")

    expect_s3_class(finbif_occurrence(), "finbif_occ")

    options(finbif_cache_path = op)

    dbDisconnect(db)

  }

)

suppressMessages(eject_cassette("finbif_occurrence_db"))
