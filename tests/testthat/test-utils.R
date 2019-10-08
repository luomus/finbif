context("Checking utilities")

test_that(
  ": reduce merge returns data.frame object when input is NULL", {

    expect_s3_class(reduce_merge(NULL), "data.frame")

  }
)
