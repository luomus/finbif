context("Checking FinBIF admin status")

test_that(
  "returns valid data", {
    expect_s3_class(finbif_admin_status(), "data.frame")
  }
)
