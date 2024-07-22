test_that("option setting works", {

  op <- options()

  options("finbif_use_cache" = NULL)

  expect_null(finbif:::.onLoad())

  options(op)

})
