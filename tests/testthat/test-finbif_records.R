context("Downloading FinBIF records")

vcr::use_cassette(
  "finbif_records", {

    test_that(
      "returns valid data", {

        records <- finbif_records(
          filters = list(primary_habitat = list(M = c("V", "H"))), n = 301
        )

        expect_output(print(records), "FinBIF")

        expect_output(print(records[[1]]), "FinBIF")

      }
    )

    test_that(
      "with count only returns valid data", {

        expect_s3_class(finbif_records(count_only = TRUE), "finbif_api")

      }
    )

  },
  preserve_exact_body_bytes = TRUE
)

test_that(
  "returns errors appropriately", {

    expect_condition(finbif_records(n = 1e99))

    expect_condition(finbif_records(filters = c(not_a_filter = TRUE)))

  }
)
